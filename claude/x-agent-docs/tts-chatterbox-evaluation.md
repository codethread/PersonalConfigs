# TTS Evaluation: Chatterbox (Hugging Face Model)

## Summary

Testing revealed that the Chatterbox TTS solution using Hugging Face models is **not viable for practical use** on my hardware due to excessive processing times. Would need resort to `say` builtin, or cloud api like elevenlabs or maybe amazon hosting

## Test Environment

### Hardware

- **Device**: MacBook M1
- **Architecture**: Apple Silicon (ARM64)
- **Memory**: System default allocation

### Software Stack

- **TTS Tool**: Chatterbox (Resemble AI)
- **Repository**: https://github.com/resemble-ai/chatterbox
- **Model**: Hugging Face Transformers-based TTS
- **Python Version**: 3.11
- **Key Dependencies**:
  - torch==2.6.0
  - torchaudio==2.6.0
  - transformers==4.46.3
  - diffusers==0.29.0

### Installation Method

- Virtual environment at `~/dev/vendor/chatterbox/venv`
- Installed via pip with custom dependency resolution
- Command-line interface: `chatterbox-tts`

## Test Methodology

We tested the TTS system with markdown-formatted text inputs of varying sizes:

- **Tiny**: ~10 tokens (simple heading with bold text)
- **Small**: ~50 tokens (paragraph with lists and formatting)
- **Medium**: ~200 tokens (multi-section document)
- **Large**: ~500 tokens (comprehensive documentation)
- **XLarge**: ~1000 tokens (full technical reference)

Input format: Piped markdown text via stdin
Output format: WAV audio files

## Test Results

### Performance Metrics

| Input Size | Tokens | Processing Time | Status  |
| ---------- | ------ | --------------- | ------- |
| Tiny       | ~10    | 67.89s          | Success |
| Small      | ~50    | 152.14s         | Success |
| Medium     | ~200   | >300s           | Timeout |

### Key Findings

1. **Excessive Processing Times**: Even minimal 10-token inputs required over 1 minute to process
2. **Non-linear Scaling**: Processing time increased dramatically with input size
3. **Practical Limit**: Inputs beyond 50 tokens exceeded reasonable wait times
4. **Token Processing Rate**: Approximately 0.15-0.33 tokens/second

## Analysis

### Performance Bottlenecks

The extreme processing times appear to be caused by:

1. **Model Complexity**: Transformer-based models require significant computation
2. **CPU-Only Processing**: M1 GPU acceleration may not be fully utilized (tested but was actually slower than CPU)
3. **Python Overhead**: Pure Python implementation without native optimizations
4. **Memory Management**: Potential inefficiencies in memory allocation

### Alternative Solutions

For viable TTS on M1 Macs, consider:

1. **Native macOS TTS**: Built-in `say` command (instant processing)
2. **Cloud APIs**: OpenAI TTS, Google Cloud TTS, Amazon Polly
3. **Optimized Local Models**: Coqui TTS, Piper TTS (better optimization)
4. **Hardware Acceleration**: Models specifically optimized for Apple Silicon

### If Continuing with Chatterbox

Would require:

- Dedicated GPU hardware (NVIDIA with CUDA)
- Server deployment rather than local execution
- Significant optimization work on the codebase
- Alternative lighter-weight models

---

**Date**: 2025-09-21
**Tester**: Claude Code Assistant
**Status**: Implementation deemed non-viable for production use
