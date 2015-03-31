[woeid | _rest ] = System.argv
temperature = CurrentWeather.YahooFetcher.fetch(woeid)

IO.puts "The current weather for woeid #{woeid} is #{temperature} degrees fahrenheit."
