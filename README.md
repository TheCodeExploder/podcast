# Podcast Studio Voice (Audacity Nyquist Plugin)

A one-click Audacity effect that applies a curated spoken-word polish chain for podcast vocals: noise gating, low-cut, breath/click taming, tone shaping, gentle de-essing, and peak limiting. This is "easy mode" with fixed settings.

## Install

1. Copy `PodcastStudioVoice.ny` to your Audacity plug-ins folder:
   - Windows: `%APPDATA%\audacity\Plug-Ins`
2. Open Audacity and go to `Effect > Add / Remove Plug-ins...`.
3. Enable **Podcast Studio Voice**.
4. Apply it from `Effect > Podcast Studio Voice`.

## Preset settings (fixed)

- Gate threshold: -45 dB
- Gate reduction: -18 dB
- Low cut: 80 Hz
- Breath/click taming: high-band gate + fast limiter
- Warmth: -2 dB @ 160 Hz
- Presence: +3 dB @ 3.5 kHz
- Air: +2 dB @ 8 kHz
- De-ess: 2 dB @ 6.5 kHz
- Limiter: -1 dB
- Output gain: 0 dB
