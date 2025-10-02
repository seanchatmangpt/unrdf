#
# AWS Certificate Manager (ACM) Configuration
# Manages TLS certificates with auto-renewal for production deployment
#

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

# Primary domain certificate (sidecar API)
resource "aws_acm_certificate" "sidecar_api" {
  domain_name               = var.sidecar_domain
  subject_alternative_names = ["*.${var.sidecar_domain}"]
  validation_method         = "DNS"

  lifecycle {
    create_before_destroy = true
  }

  tags = {
    Name        = "UNRDF Sidecar API Certificate"
    Environment = var.environment
    ManagedBy   = "Terraform"
    Service     = "sidecar"
  }
}

# DNS validation records (Route53)
resource "aws_route53_record" "cert_validation" {
  for_each = {
    for dvo in aws_acm_certificate.sidecar_api.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.route53_zone_id
}

# Certificate validation
resource "aws_acm_certificate_validation" "sidecar_api" {
  certificate_arn         = aws_acm_certificate.sidecar_api.arn
  validation_record_fqdns = [for record in aws_route53_record.cert_validation : record.fqdn]

  timeouts {
    create = "10m"
  }
}

# Client certificate for mTLS (if using AWS Private CA)
resource "aws_acmpca_certificate_authority" "mtls_ca" {
  count = var.enable_mtls ? 1 : 0

  type = "ROOT"

  certificate_authority_configuration {
    key_algorithm     = "RSA_4096"
    signing_algorithm = "SHA512WITHRSA"

    subject {
      common_name         = "UNRDF mTLS CA"
      organization        = "UNRDF"
      organizational_unit = "Security"
      country             = "US"
      state               = "California"
      locality            = "San Francisco"
    }
  }

  permanent_deletion_time_in_days = 7

  tags = {
    Name        = "UNRDF mTLS Certificate Authority"
    Environment = var.environment
    ManagedBy   = "Terraform"
    Service     = "mtls"
  }
}

# CA certificate
resource "aws_acmpca_certificate" "mtls_ca_cert" {
  count = var.enable_mtls ? 1 : 0

  certificate_authority_arn   = aws_acmpca_certificate_authority.mtls_ca[0].arn
  certificate_signing_request = aws_acmpca_certificate_authority.mtls_ca[0].certificate_signing_request
  signing_algorithm           = "SHA512WITHRSA"

  template_arn = "arn:aws:acm-pca:::template/RootCACertificate/V1"

  validity {
    type  = "YEARS"
    value = 10
  }
}

# Install CA certificate
resource "aws_acmpca_certificate_authority_certificate" "mtls_ca_cert_install" {
  count = var.enable_mtls ? 1 : 0

  certificate_authority_arn = aws_acmpca_certificate_authority.mtls_ca[0].arn
  certificate               = aws_acmpca_certificate.mtls_ca_cert[0].certificate
  certificate_chain         = aws_acmpca_certificate.mtls_ca_cert[0].certificate_chain
}

# Client certificate template
resource "aws_acmpca_permission" "mtls_client_cert_permission" {
  count = var.enable_mtls ? 1 : 0

  certificate_authority_arn = aws_acmpca_certificate_authority.mtls_ca[0].arn
  actions                   = ["IssueCertificate", "GetCertificate", "ListPermissions"]
  principal                 = "acm.amazonaws.com"
}

# CloudWatch alarms for certificate expiry
resource "aws_cloudwatch_metric_alarm" "cert_expiry_warning" {
  alarm_name          = "sidecar-cert-expiry-warning-${var.environment}"
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = "1"
  metric_name         = "DaysToExpiry"
  namespace           = "AWS/CertificateManager"
  period              = "86400" # 1 day
  statistic           = "Minimum"
  threshold           = "30" # Alert 30 days before expiry
  alarm_description   = "Certificate expiring in less than 30 days"
  treat_missing_data  = "notBreaching"

  dimensions = {
    CertificateArn = aws_acm_certificate.sidecar_api.arn
  }

  alarm_actions = [var.sns_topic_arn]
}

# Outputs
output "certificate_arn" {
  description = "ARN of the sidecar API certificate"
  value       = aws_acm_certificate.sidecar_api.arn
}

output "certificate_domain" {
  description = "Domain name of the certificate"
  value       = aws_acm_certificate.sidecar_api.domain_name
}

output "mtls_ca_arn" {
  description = "ARN of the mTLS Certificate Authority"
  value       = var.enable_mtls ? aws_acmpca_certificate_authority.mtls_ca[0].arn : null
}

# Variables
variable "sidecar_domain" {
  description = "Primary domain for sidecar API"
  type        = string
}

variable "route53_zone_id" {
  description = "Route53 hosted zone ID for DNS validation"
  type        = string
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
}

variable "enable_mtls" {
  description = "Enable mTLS with Private CA"
  type        = bool
  default     = false
}

variable "sns_topic_arn" {
  description = "SNS topic ARN for certificate expiry alerts"
  type        = string
}
