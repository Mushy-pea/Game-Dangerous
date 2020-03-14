int loadBuffer(int mode, int limit, int* waveFile, int* pBuf)
{
int lastBlock;
if (mode == 0) {
  waveFile = waveFile + 40;
  lastBlock = (*waveFile) / 4 - 1;
  waveFile = waveFile + 2;
  loadBuffer(1, lastBlock, waveFile, pBuf)
}
else {
  for (int n = 0; n <= limit; n++) {
    *pBuf = *waveFile;
    waveFile = waveFile + 4;
    pBuf = pBuf + 4;
}
}
}
