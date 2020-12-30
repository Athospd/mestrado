# Uncomment the following line to run in Google Colab
# !pip install torchaudio 
import torch
import torchaudio
from torchaudio import functional as F
from torchaudio import functional as F
from torchaudio.compliance.kaldi import *
import requests
import matplotlib.pyplot as plt
import numpy as np
import math

waveform = torch.rand(1000)

m = torchaudio.transforms.MFCC()(x)


sample_rate = 16000
n_mfcc = 40
dct_type = 2
norm = 'ortho'
log_mels = False


sample_rate = sample_rate
n_mfcc = n_mfcc
dct_type = dct_type
norm = norm
top_db = 80.0
amplitude_to_DB = torchaudio.transforms.AmplitudeToDB('power', top_db)
MelSpectrogram = torchaudio.transforms.MelSpectrogram(sample_rate=sample_rate)

dct_mat = F.create_dct(n_mfcc, MelSpectrogram.n_mels, norm)


tm = torchaudio.transforms.MFCC()
a = tm(torch.tensor(np.sin(np.arange(1,1000)), dtype = torch.float))

r.a = a

# pack batch
shape = waveform.size()
waveform = waveform.reshape(-1, shape[-1])

mel_specgram = MelSpectrogram(waveform)
if log_mels:
    log_offset = 1e-6
    mel_specgram = torch.log(mel_specgram + log_offset)
else:
    mel_specgram = amplitude_to_DB(mel_specgram)
# (channel, n_mels, time).tranpose(...) dot (n_mels, n_mfcc)
# -> (channel, time, n_mfcc).tranpose(...)
mfcc = torch.matmul(mel_specgram.transpose(1, 2), dct_mat).transpose(1, 2)

# unpack batch
mfcc = mfcc.reshape(shape[:-1] + mfcc.shape[-2:])

return mfcc
