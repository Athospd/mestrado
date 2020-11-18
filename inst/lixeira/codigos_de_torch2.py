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


upsample_scales = [2,2,5]
n_classes = 5
hop_length = 20
n_res_block = 10
n_rnn = 512
n_fc = 512
kernel_size = 5
n_freq = 128
n_hidden = 128
n_output = 128
waveform = torch.rand(3,1,19200)
specgram = torch.rand(3,1,128,100)


kernel_size = kernel_size
n_rnn = n_rnn
n_aux = n_output // 4
hop_length = hop_length
n_classes = n_classes

total_scale = 1
for upsample_scale in upsample_scales:
    total_scale *= upsample_scale
if total_scale != hop_length:
    raise ValueError(f"Expected: total_scale == hop_length, but found {total_scale} != {hop_length}")

upsample = UpsampleNetwork(upsample_scales,
                                n_res_block,
                                n_freq,
                                n_hidden,
                                n_output,
                                kernel_size)
fc = nn.Linear(n_freq + n_aux + 1, n_rnn)

rnn1 = nn.GRU(n_rnn, n_rnn, batch_first=True)
rnn2 = nn.GRU(n_rnn + n_aux, n_rnn, batch_first=True)

relu1 = nn.ReLU(inplace=True)
relu2 = nn.ReLU(inplace=True)

fc1 = nn.Linear(n_rnn + n_aux, n_fc)
fc2 = nn.Linear(n_fc + n_aux, n_fc)
fc3 = nn.Linear(n_fc, n_classes)

assert waveform.size(1) == 1, 'Require the input channel of waveform is 1'
assert specgram.size(1) == 1, 'Require the input channel of specgram is 1'
# remove channel dimension until the end
waveform, specgram = waveform.squeeze(1), specgram.squeeze(1)

batch_size = waveform.size(0)
h1 = torch.zeros(1, batch_size, n_rnn, dtype=waveform.dtype, device=waveform.device)
h2 = torch.zeros(1, batch_size, n_rnn, dtype=waveform.dtype, device=waveform.device)
# output of upsample:
# specgram: (n_batch, n_freq, (n_time - kernel_size + 1) * total_scale)
# aux: (n_batch, n_output, (n_time - kernel_size + 1) * total_scale)
specgram, aux = upsample(specgram)
specgram = specgram.transpose(1, 2)
aux = aux.transpose(1, 2)

aux_idx = [n_aux * i for i in range(5)]
a1 = aux[:, :, aux_idx[0]:aux_idx[1]]
a2 = aux[:, :, aux_idx[1]:aux_idx[2]]
a3 = aux[:, :, aux_idx[2]:aux_idx[3]]
a4 = aux[:, :, aux_idx[3]:aux_idx[4]]

x = torch.cat([waveform.unsqueeze(-1), specgram, a1], dim=-1)
x = fc(x)
res = x
x, _ = rnn1(x, h1)

x = x + res
res = x
x = torch.cat([x, a2], dim=-1)
x, _ = rnn2(x, h2)

x = x + res
x = torch.cat([x, a3], dim=-1)
x = fc1(x)
x = relu1(x)

x = torch.cat([x, a4], dim=-1)
x = fc2(x)
x = relu2(x)
x = fc3(x)

# bring back channel dimension
return x.unsqueeze(1)




upsamplenetwork = torchaudio.models.WaveRNN(upsample_scales=c(4, 4, 16))
input = torch::torch_rand (10, 128, 10)  # a random spectrogram
output = upsamplenetwork (input)  # shape: (10, 1536, 128), (10, 1536, 128)
expect_equal(dim(output[[1]]), c(10, 1536, 128))
expect_equal(dim(output[[2]]), c(10, 1536, 128))
