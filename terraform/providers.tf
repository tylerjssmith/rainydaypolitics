##### Terraform ###############################################################
terraform {
  required_version = ">= 1.5.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 6.18"
    }
  }
}

##### Provider: AWS ###########################################################
provider "aws" {
  region = var.region_name
  default_tags {
    tags = {
      Project   = var.project_name
      ManagedBy = "terraform"
    }
  }
}