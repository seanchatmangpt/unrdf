output "cluster_name" {
  description = "EKS cluster name"
  value       = module.eks.cluster_name
}

output "cluster_endpoint" {
  description = "EKS cluster endpoint"
  value       = module.eks.cluster_endpoint
  sensitive   = true
}

output "cluster_certificate_authority_data" {
  description = "EKS cluster certificate authority data"
  value       = module.eks.cluster_certificate_authority_data
  sensitive   = true
}

output "cluster_security_group_id" {
  description = "EKS cluster security group ID"
  value       = module.eks.cluster_security_group_id
}

output "node_group_id" {
  description = "EKS node group ID"
  value       = module.eks.eks_managed_node_groups["knowd"].node_group_id
}

output "node_group_status" {
  description = "EKS node group status"
  value       = module.eks.eks_managed_node_groups["knowd"].node_group_status
}

output "kubeconfig" {
  description = "Kubeconfig for accessing the cluster"
  value       = module.eks.kubeconfig
  sensitive   = true
}

output "cluster_oidc_issuer_url" {
  description = "OIDC issuer URL for the cluster"
  value       = module.eks.cluster_oidc_issuer_url
}
