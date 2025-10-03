output "vpc_id" {
  description = "VPC ID"
  value       = module.vpc.vpc_id
}

output "vpc_cidr_block" {
  description = "VPC CIDR block"
  value       = module.vpc.vpc_cidr_block
}

output "private_subnets" {
  description = "Private subnet IDs"
  value       = module.vpc.private_subnets
}

output "public_subnets" {
  description = "Public subnet IDs"
  value       = module.vpc.public_subnets
}

output "nat_gateway_ids" {
  description = "NAT gateway IDs"
  value       = module.vpc.natgw_ids
}

output "internet_gateway_id" {
  description = "Internet gateway ID"
  value       = module.vpc.igw_id
}

output "eks_security_group_id" {
  description = "Security group ID for EKS"
  value       = aws_security_group.eks.id
}
