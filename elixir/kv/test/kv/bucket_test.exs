defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  setup do
    { :ok, bucket } = KV.Bucket.start_link
    { :ok, bucket: bucket }
  end

  test "stores the value by key", %{ bucket: bucket } do
    assert KV.Bucket.get(bucket, "milk") == nil

    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
  end

  test "deletes previously stored value", %{ bucket: bucket } do
    KV.Bucket.put(bucket, "milk", 3)
    KV.Bucket.delete(bucket, "milk")

    assert KV.Bucket.get(bucket, "milk") == nil
  end
end