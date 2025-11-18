##### Networking ##############################################################
# Virtual Private Cloud
data "aws_vpc" "default" {
  default = true
}

# Subnets
data "aws_subnets" "default_vpc_subnets" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

resource "aws_db_subnet_group" "rds_subnets" {
  name       = "rds-default-subnets"
  subnet_ids = data.aws_subnets.default_vpc_subnets.ids
}

##### Security Groups #########################################################
# EC2 Web Server
resource "aws_security_group" "ec2_web_sg" {
  name        = "${var.project_name}-ec2-web-sg"
  description = "allow HTTP and HTTPS"
  vpc_id      = data.aws_vpc.default.id
}

resource "aws_vpc_security_group_ingress_rule" "ec2_web_sg_ingress_http" {
  security_group_id = aws_security_group.ec2_web_sg.id

  description = "HTTP"
  from_port   = 80
  to_port     = 80
  ip_protocol = "tcp"
  cidr_ipv4   = "0.0.0.0/0"
}

resource "aws_vpc_security_group_ingress_rule" "ec2_web_sg_ingress_https" {
  security_group_id = aws_security_group.ec2_web_sg.id

  description = "HTTPS"
  from_port   = 443
  to_port     = 443
  ip_protocol = "tcp"
  cidr_ipv4   = "0.0.0.0/0"
}

resource "aws_vpc_security_group_egress_rule" "ec2_web_sg_egress" {
  security_group_id = aws_security_group.ec2_web_sg.id

  ip_protocol = "-1"
  cidr_ipv4   = "0.0.0.0/0"
}

# RDS Database
resource "aws_security_group" "rds_sg" {
  name        = "${var.project_name}-rds-sg"
  description = "allow Postgres from web server"
  vpc_id      = data.aws_vpc.default.id
}

resource "aws_vpc_security_group_ingress_rule" "rds_sg_ingress_from_ec2_web" {
  security_group_id = aws_security_group.rds_sg.id

  description                  = "allow Postgres from web server"
  from_port                    = 5432
  to_port                      = 5432
  ip_protocol                  = "tcp"
  referenced_security_group_id = aws_security_group.ec2_web_sg.id
}

resource "aws_vpc_security_group_egress_rule" "rds_sg_egress" {
  security_group_id = aws_security_group.rds_sg.id

  ip_protocol = "-1"
  cidr_ipv4   = "0.0.0.0/0"
}

##### EC2 Web Server ##########################################################
# EC2 Web Server
resource "aws_instance" "web_server" {
  ami                         = var.ec2_web_ami
  instance_type               = var.ec2_web_type
  vpc_security_group_ids      = [aws_security_group.ec2_web_sg.id]
  subnet_id                   = data.aws_subnets.default_vpc_subnets.ids[0]
  associate_public_ip_address = false
  key_name                    = var.ec2_web_existing_ssh_key_name

  root_block_device {
    volume_type           = "gp3"
    volume_size           = 16
    encrypted             = true
    delete_on_termination = true

    tags = {
      Name = "${var.ec2_web_name}-ebs"
    }
  }

  tags = {
    Name = var.ec2_web_name
  }
}

# Elastic IP Address
data "aws_eip" "existing_web_eip" {
  id = var.ec2_web_existing_eip_id
}

resource "aws_eip_association" "web_eip_assoc" {
  instance_id   = aws_instance.web_server.id
  allocation_id = data.aws_eip.existing_web_eip.id
}

##### RDS Database ############################################################
# RDS Database
resource "aws_db_instance" "postgres" {
  identifier        = "${var.project_name}-rds"
  engine            = "postgres"
  engine_version    = var.db_engine_version
  instance_class    = var.db_instance_class
  allocated_storage = var.db_allocated_storage
  storage_type      = "gp3"

  db_name                     = var.db_name
  username                    = var.db_user
  manage_master_user_password = true

  db_subnet_group_name   = aws_db_subnet_group.rds_subnets.name
  vpc_security_group_ids = [aws_security_group.rds_sg.id]
  publicly_accessible    = false

  backup_retention_period  = 1
  delete_automated_backups = true
  deletion_protection      = false
  skip_final_snapshot      = true

  auto_minor_version_upgrade      = true
  copy_tags_to_snapshot           = true
  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]
}