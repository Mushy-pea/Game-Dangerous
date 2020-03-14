void loadBuffer(int mode, int limit, int* pBuf, int* waveFile)
{
int lastBlock;
if (mode == 0) {
  waveFile = waveFile + 10;
  lastBlock = (*waveFile) / 4 - 1;
  waveFile = waveFile + 1;
  loadBuffer(1, lastBlock, pBuf, waveFile);
}
else {
  for (int n = 0; n <= limit; n++) {
    *pBuf = *waveFile;
    waveFile = waveFile + 1;
    pBuf = pBuf + 1;
}
}
}
