port = Port.open({:spawn, "pacat -p --channels=1 --rate=16000 --format=s16be"}, [:binary])

diddy = "BAGABBBAAABDD"

notes = %{
  ?A => 440,
  ?B => 493.88,
  ?G => 392,
  ?D => 587.33
}

duration = 0.5

for <<note <- diddy>> do
  frequency = notes[note]
  IO.puts "playing #{frequency}Hz for #{duration} seconds"

  w = %SineWave{frequency: frequency}
  data = PcmSampler.sample(w, duration)

  send(port, {self, {:command, data}})
end
