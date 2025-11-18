##### EC2 Web Server ##########################################################
output "WEB_INSTANCE_ID" {
  value       = aws_instance.web_server.id
  description = "web server instance id"
}

output "WEB_PUB_IP" {
  value       = data.aws_eip.existing_web_eip.public_ip
  description = "web server public ip address (EIP)"
}

##### RDS Database ############################################################
output "RDS_HOST" {
  value       = aws_db_instance.postgres.address
  description = "rds hostname"
}

output "RDS_PORT" {
  value       = aws_db_instance.postgres.port
  description = "rds port"
}

output "RDS_USER" {
  value       = aws_db_instance.postgres.username
  description = "rds username"
}

output "RDS_DB_NAME" {
  value       = aws_db_instance.postgres.db_name
  description = "rds database"
}

output "RDS_MASTER_SECRET_ARN" {
  value       = aws_db_instance.postgres.master_user_secret[0].secret_arn
  description = "rds admin credentials arn"
  sensitive   = true
}

##### Observability ###########################################################
output "SNS_TOPIC_ARN" {
  description = "sns topic arn"
  value       = aws_sns_topic.observability_alerts.arn
}