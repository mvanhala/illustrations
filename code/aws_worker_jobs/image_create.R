library(awsjobs)

library(stringr)

ssm_iam_profile <- Sys.getenv("AWS_R_SSM_PROFILE")
security_group_id <- Sys.getenv("AWS_R_SECURITY_GROUP")

ubuntu_18_04_ami <- "ami-0a634ae95e11c6f91"
instance_type <- "m5a.large"
spot <- TRUE
spot_max_price <- 0.10
region <- "us-west-2"

user_data <- readr::read_file("code/aws_worker_jobs/image_install.sh") %>%
  charToRaw() %>%
  base64enc::base64encode()

config <- list(region = region)
svc_ec2 <- paws::ec2(config = config)
svc_ssm <- paws::ssm(config = config)


instance <- svc_ec2$run_instances(
  MinCount = 1, 
  MaxCount = 1,
  ImageId = ubuntu_18_04_ami,
  InstanceType = instance_type,
  SecurityGroupIds = security_group_id,
  IamInstanceProfile = list(Name = ssm_iam_profile),
  TagSpecifications = list(
    list(
      ResourceType = "instance",
      Tags = list(
        list(
          Key = "Name",
          Value = glue::glue("R-image-create")
        )
      )
    )
  ),
  InstanceMarketOptions =  list(
    MarketType = "spot",
    SpotOptions = list(
      MaxPrice = spot_max_price,
      SpotInstanceType = "one-time"
    )
  ),
  UserData = user_data
)


instance_id <- instance$Instances[[1]]$InstanceId


check_status <- function(instance_id, n = 5) {
  status_cmd <- svc_ssm$send_command(
    InstanceIds = list(instance_id),
    DocumentName = "AWS-RunShellScript",
    Parameters = list(
      commands = list(
        glue::glue("tail -{n} /var/log/cloud-init-output.log")
      )
    ),
    Comment = glue::glue("Check status of user data on {instance_id}")
  )
  
  status_cmd_id <- status_cmd$Command$CommandId
  
  while (TRUE) {
    Sys.sleep(0.5)
    result <- svc_ssm$list_command_invocations(
      CommandId = status_cmd_id, 
      InstanceId = instance_id, 
      Details = TRUE
    )
    exec_status <- result$CommandInvocations[[1]]$StatusDetails
    terminal_status <- c("Success", "DeliveryTimedOut", "ExecutionTimedOut", 
                         "Failed", "Canceled", "Undeliverable", "Terminated")
    if (exec_status %in% terminal_status) break
    Sys.sleep(0.5)
  }
  
  output <- result$CommandInvocations[[1]]$CommandPlugins[[1]]$Output
  cat(output)
  invisible(output)
}

monitor_status <- function(instance_id, n = 5) {
  while (TRUE) {
    cat(strftime(Sys.time()), "\n")
    status <- check_status(instance_id, n)
    last_line <- status %>%
      str_trim() %>%
      str_split("\n") %>%
      .[[1]] %>%
      .[[length(.)]]
    if (str_detect(last_line, "^Cloud-init.+finished")) break
    Sys.sleep(30)
  }
}

monitor_status(instance_id)


image_ami <- svc_ec2$create_image(
  Description = "R 3.6.3 + packages",
  InstanceId = instance_id, 
  Name = glue::glue("R-3.6.3")
)

svc_ec2$create_tags(
  Resources = list(image_ami$ImageId), 
  Tags = list(list(Key = "Name", Value = "R-3.6.3"))
)

svc_ec2$terminate_instances(InstanceIds = list(instance_id))


