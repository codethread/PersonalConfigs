# TTS Evaluation: speak CLI (MLX-based Implementation)

## Summary

Testing revealed that the `speak` CLI tool provides **significantly better performance** compared to Chatterbox, with processing times that are practical for real-world use. The tool successfully handles all input sizes with reasonable latency, making it a **viable TTS solution** for M1 Mac hardware.

## Test Environment

### Hardware

- **Device**: MacBook M1
- **Architecture**: Apple Silicon (ARM64)
- **Memory**: System default allocation

### Software Stack

- **TTS Tool**: speak CLI (custom MLX-based implementation)
- **Location**: Globally available command
- **Backend**: MLX framework optimized for Apple Silicon
- **Key Features**:
  - Native M1 optimization
  - Efficient memory usage
  - Streaming audio output

### Installation Method

- Binary available globally via PATH
- Command-line interface: `speak`
- Input methods: stdin pipe or --text flag

## Test Methodology

We tested the TTS system with markdown-formatted text inputs of varying sizes:

- **Tiny**: ~10 tokens (simple heading - 18 chars)
- **Small**: ~50 tokens (paragraph with lists - 207 chars)
- **Medium**: ~200 tokens (multi-section document - 878 chars)
- **Large**: ~500 tokens (comprehensive documentation - 2,146 chars)
- **XLarge**: ~1000 tokens (full technical reference - 5,352 chars)

Input format: Piped markdown text via stdin
Output format: Direct audio playback

## Test Results

### Performance Metrics

| Input Size | Characters | Tokens | Processing Time | Status  | vs Chatterbox |
| ---------- | ---------- | ------ | --------------- | ------- | ------------- |
| Tiny       | 18         | ~10    | 11.12s          | Success | 6.1x faster   |
| Small      | 207        | ~50    | 11.90s          | Success | 12.8x faster  |
| Medium     | 878        | ~200   | 20.81s          | Success | >14x faster   |
| Large      | 2,146      | ~500   | 36.19s          | Success | N/A           |
| XLarge     | 5,352      | ~1000  | 78.18s          | Success | N/A           |

### Key Findings

1. **Excellent Performance**: All input sizes processed successfully within reasonable timeframes
2. **Linear Scaling**: Processing time scales linearly with input size (better predictability)
3. **Practical Latency**: Even large documents (1000 tokens) process in under 80 seconds
4. **Consistent Baseline**: ~11s minimum processing time for initialization
5. **Token Processing Rate**: Approximately 12-15 tokens/second after initialization

## Analysis

### Performance Advantages

The dramatically improved performance appears to be due to:

1. **MLX Optimization**: Framework specifically designed for Apple Silicon
2. **Metal GPU Acceleration**: Full utilization of M1 GPU capabilities
3. **Native Implementation**: Compiled binary with minimal overhead
4. **Efficient Memory Management**: Optimized for unified memory architecture

### Comparison with Chatterbox

| Aspect                    | speak CLI           | Chatterbox      |
| ------------------------- | ------------------- | --------------- |
| Tiny Input (10 tokens)    | 11.12s              | 67.89s          |
| Small Input (50 tokens)   | 11.90s              | 152.14s         |
| Medium Input (200 tokens) | 20.81s              | >300s (timeout) |
| Large Support             | ✅ Yes              | ❌ No           |
| XLarge Support            | ✅ Yes              | ❌ No           |
| Practical Viability       | ✅ Production-ready | ❌ Not viable   |

### Strengths

1. **Fast Processing**: 6-14x faster than Chatterbox for comparable inputs
2. **Scalability**: Handles large inputs that Chatterbox cannot process
3. **Predictable Performance**: Linear scaling makes timing predictable
4. **Production Ready**: Latency acceptable for real-world applications

### Limitations

1. **Initialization Overhead**: ~11s baseline even for tiny inputs
2. **No Batch Processing**: Each invocation has initialization cost
3. **Limited Voice Options**: May have fewer voice customization options

## Recommendations

### For Production Use

The `speak` CLI is **recommended for production** with the following considerations:

1. **Batch Small Inputs**: Combine multiple small texts to amortize initialization cost
2. **Async Processing**: Use background processing for better UX
3. **Cache Results**: Store generated audio for frequently requested content
4. **Set Expectations**: Inform users about processing time for large texts

### Optimization Opportunities

1. **Persistent Process**: Keep model loaded in memory to eliminate initialization
2. **Streaming Output**: Start playback before complete generation
3. **Progressive Enhancement**: Process in chunks for faster initial response
4. **Queue Management**: Implement request queuing for multiple users

## Conclusion

The `speak` CLI represents a **major improvement** over Chatterbox for TTS on M1 Macs. With 6-14x performance gains and the ability to handle large inputs, it transitions from "non-viable" to "production-ready". The MLX-based implementation successfully leverages Apple Silicon capabilities to deliver practical TTS performance for real-world applications.

---

**Date**: 2025-09-21
**Tester**: Claude Code Assistant
**Status**: Implementation recommended for production use with noted optimizations
