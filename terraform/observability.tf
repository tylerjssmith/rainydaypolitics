##### CloudWatch Dashboard ####################################################
resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${var.project_name}-dashboard"

  dashboard_body = jsonencode({
    widgets = [
      {
        type = "metric"
        x    = 0
        y    = 0
        width  = 12
        height = 6
        properties = {
          title  = "EC2 CPUUtilization"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/EC2", "CPUUtilization", "InstanceId", aws_instance.web_server.id ]
          ]
          stat   = "Average"
          period = 300
        }
      },
      {
        type = "metric"
        x    = 12
        y    = 0
        width  = 12
        height = 6
        properties = {
          title  = "EC2 Status Check Failed"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/EC2", "StatusCheckFailed", "InstanceId", aws_instance.web_server.id ]
          ]
          stat   = "Maximum"
          period = 60
        }
      },
      {
        type = "metric"
        x    = 0
        y    = 6
        width  = 12
        height = 6
        properties = {
          title  = "RDS CPUUtilization"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/RDS", "CPUUtilization", "DBInstanceIdentifier", aws_db_instance.postgres.id ]
          ]
          stat   = "Average"
          period = 300
        }
      },
      {
        type = "metric"
        x    = 12
        y    = 6
        width  = 12
        height = 6
        properties = {
          title  = "RDS FreeStorageSpace"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/RDS", "FreeStorageSpace", "DBInstanceIdentifier", aws_db_instance.postgres.id ]
          ]
          stat   = "Average"
          period = 300
          yAxis = {
            left = {
              min = 0
            }
          }
        }
      },
      {
        type = "metric"
        x    = 0
        y    = 12
        width  = 12
        height = 6
        properties = {
          title  = "RDS DatabaseConnections"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/RDS", "DatabaseConnections", "DBInstanceIdentifier", aws_db_instance.postgres.id ]
          ]
          stat   = "Average"
          period = 300
        }
      },
      {
        type = "metric"
        x    = 12
        y    = 12
        width  = 12
        height = 6
        properties = {
          title  = "RDS FreeableMemory"
          view   = "timeSeries"
          region = var.region_name
          metrics = [
            [ "AWS/RDS", "FreeableMemory", "DBInstanceIdentifier", aws_db_instance.postgres.id ]
          ]
          stat   = "Average"
          period = 300
        }
      }
    ]
  })
}

##### SNS for CloudWatch Alarms ###############################################
resource "aws_sns_topic" "observability_alerts" {
  name = "${var.project_name}-alerts"
}

resource "aws_sns_topic_subscription" "observability_email" {
  topic_arn = aws_sns_topic.observability_alerts.arn
  protocol  = "email"
  endpoint  = var.alarm_email
}

##### EC2 CloudWatch Alarms ###################################################
# EC2 High CPU
resource "aws_cloudwatch_metric_alarm" "ec2_high_cpu" {
  alarm_name          = "${var.project_name}-ec2-high-cpu"
  alarm_description   = "CPUUtilization > 70% for 10 minutes on EC2 instance ${aws_instance.web_server.id}"
  namespace           = "AWS/EC2"
  metric_name         = "CPUUtilization"
  comparison_operator = "GreaterThanThreshold"
  statistic           = "Average"
  period              = 300
  evaluation_periods  = 2
  threshold           = 70
  treat_missing_data  = "missing"

  dimensions = {
    InstanceId = aws_instance.web_server.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

# EC2 Status Check Failed
resource "aws_cloudwatch_metric_alarm" "ec2_status_check_failed" {
  alarm_name          = "${var.project_name}-ec2-status-check-failed"
  alarm_description   = "status check failed for EC2 instance ${aws_instance.web_server.id}"
  namespace           = "AWS/EC2"
  metric_name         = "StatusCheckFailed"
  comparison_operator = "GreaterThanThreshold"
  statistic           = "Maximum"
  period              = 60
  evaluation_periods  = 2
  threshold           = 0
  treat_missing_data  = "missing"

  dimensions = {
    InstanceId = aws_instance.web_server.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

##### RDS CloudWatch Alarms ###################################################
# RDS High CPU
resource "aws_cloudwatch_metric_alarm" "rds_high_cpu" {
  alarm_name          = "${var.project_name}-rds-high-cpu"
  alarm_description   = "CPUUtilization > 70% for 10 minutes on ${aws_db_instance.postgres.id}"
  namespace           = "AWS/RDS"
  metric_name         = "CPUUtilization"
  comparison_operator = "GreaterThanThreshold"
  statistic           = "Average"
  period              = 300
  evaluation_periods  = 2
  threshold           = 70
  treat_missing_data  = "missing"

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

# RDS Low CPU Credit Balance
resource "aws_cloudwatch_metric_alarm" "rds_low_cpu_credits" {
  alarm_name          = "${var.project_name}-rds-low-cpu-credits"
  alarm_description   = "RDS CPU credit balance low for burstable instance ${aws_db_instance.postgres.id}"
  namespace           = "AWS/RDS"
  metric_name         = "CPUCreditBalance"
  comparison_operator = "LessThanThreshold"
  statistic           = "Average"
  period              = 300       # 5 minutes
  evaluation_periods  = 2         # 10 minutes
  threshold           = 30        # alert when < 30 credits
  treat_missing_data  = "missing"

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

# RDS Low FreeableMemory
resource "aws_cloudwatch_metric_alarm" "rds_low_freeable_memory" {
  alarm_name          = "${var.project_name}-rds-low-freeable-memory"
  alarm_description   = "RDS FreeableMemory below 200 MB for 10 minutes on ${aws_db_instance.postgres.id}"
  namespace           = "AWS/RDS"
  metric_name         = "FreeableMemory"
  comparison_operator = "LessThanThreshold"
  statistic           = "Average"
  period              = 300
  evaluation_periods  = 2
  threshold           = 200000000
  treat_missing_data  = "missing"

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

# RDS Low FreeStorageSpace
resource "aws_cloudwatch_metric_alarm" "rds_low_free_storage" {
  alarm_name          = "${var.project_name}-rds-low-free-storage"
  alarm_description   = "RDS FreeStorageSpace below 10 GB for 10 minutes on ${aws_db_instance.postgres.id}"
  namespace           = "AWS/RDS"
  metric_name         = "FreeStorageSpace"
  comparison_operator = "LessThanThreshold"
  statistic           = "Average"
  period              = 300
  evaluation_periods  = 2
  threshold           = 10000000000
  treat_missing_data  = "missing"

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}

# RDS Database Connections
resource "aws_cloudwatch_metric_alarm" "rds_connections" {
  alarm_name          = "${var.project_name}-rds-connections"
  alarm_description   = "DatabaseConnections > 3 for 10 minutes on ${aws_db_instance.postgres.id}"
  namespace           = "AWS/RDS"
  metric_name         = "DatabaseConnections"
  comparison_operator = "GreaterThanThreshold"
  statistic           = "Maximum"
  period              = 300
  evaluation_periods  = 2
  threshold           = 3
  treat_missing_data  = "missing"

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.postgres.id
  }

  alarm_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  ok_actions = [
    aws_sns_topic.observability_alerts.arn
  ]

  tags = {
    Project = var.project_name
  }
}