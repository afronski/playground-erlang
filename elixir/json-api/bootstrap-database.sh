#!/bin/sh

mix ecto.create
MIX_ENV=test mix ecto.create

mix ecto.migrate
MIX_ENV=test mix ecto.migrate
