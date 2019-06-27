import os

for note in range(49):
    t0 = note * 2
    cmd = 'ffmpeg -i piano-c2-c6-2sec.mp3 -ss {} -t 2 piano-note{}-2sec.mp3'.format(t0, note+36)
    print cmd
    os.system(cmd)
