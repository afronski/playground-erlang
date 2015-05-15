defmodule ErlcloudPlaygroundTest do
  use ExUnit.Case

  test "should list all EC2 images" do
    assert Enum.count(:erlcloud_ec2.describe_images) == 0
  end

  test "should list all S3 buckets" do
    assert Enum.count(:erlcloud_s3.list_buckets) == 0
  end
end
