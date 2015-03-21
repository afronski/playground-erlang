#!/bin/sh

docker run --name json_api_postgres -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres

sleep 10

if [ ! -f priv/repo/migrations/*_create_contacts* ]; then
    mix ecto.gen.migration create_contacts
fi

./bootstrap-database.sh
