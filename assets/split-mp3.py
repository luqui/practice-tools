import os

names = [
    "kick",
    "snare1",
    "snare2",
    "snare3",
    "hat1",
    "hat2",
    "hat3",
    "tom1",
    "tom2",
    "tom3",
    "ride1",
    "ride2",
    "ride3",
    "crash" ]

t0 = 0
for drum in names:
    for vol in ['soft','loud']:
        cmd = 'ffmpeg -i DrumSamples.mp3 -ss {} -t 1 Drum-{}-{}.mp3'.format(t0, drum, vol);
        print cmd
        os.system(cmd)
        t0 += 1
