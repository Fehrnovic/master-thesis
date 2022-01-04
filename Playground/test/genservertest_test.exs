defmodule GenservertestTest do
  use ExUnit.Case
  doctest Genservertest

  test "greets the world" do
    assert Genservertest.hello() == :world
  end
end
