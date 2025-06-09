# OpenAI Backend for AI Mode

## Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Installation](#installation)
  - [Git Installation](#git-installation)
- [Configuration](#configuration)
- [Usage](#usage)
  - [Model Selection](#model-selection)
  - [Integration with AI Mode Features](#integration-with-ai-mode-features)
  - [Advanced Configuration](#advanced-configuration)
  - [Adding Custom Models](#adding-custom-models)
- [Related Resources](#related-resources)
  - [AI Mode Ecosystem](#ai-mode-ecosystem)
  - [Documentation and Community](#documentation-and-community)
- [Legal Notice](#legal-notice)

## Overview

The OpenAI backend for `ai-mode` provides seamless integration with OpenAI's language models through their API. It serves as a bridge between `ai-mode`'s powerful AI features and OpenAI's cutting-edge models, enabling you to leverage GPT-4o, o1, and other variants within your Emacs environment.

## Key Features
- **Multiple model support**: Access various OpenAI models including GPT-4o, o1, and their variants through a unified interface
- **Flexible configuration**: Customize temperature, max tokens, and other parameters per model to fine-tune AI responses
- **Native API integration**: Direct communication with OpenAI's API using proper authentication and request handling
- **Asynchronous operation**: Non-blocking API calls ensure smooth editor performance during AI interactions
- **Model management**: Easy model selection and switching between different OpenAI offerings

This backend plugin enables `ai-mode` to harness the power of OpenAI's language models, providing the necessary infrastructure for API communication, authentication, and response processing within the broader `ai-mode` ecosystem.

## Installation

### Git Installation

To get started, clone the repository into your Emacs plugins directory:

```bash
cd ~/.emacs.d/plugins
git clone --recursive https://github.com/ai-mode/ai-mode-openai
```

Next, update your `.emacs` configuration file to include the new plugin:

```elisp
(add-to-list 'load-path "~/.emacs.d/plugins/ai-mode-openai")
(require 'ai-mode-openai)
```

Alternatively, you can use `use-package` for a more modular setup:

```elisp
(use-package ai-mode-openai
  :load-path "~/.emacs.d/plugins/ai-mode-openai"
  :config
  (setq ai-mode--models-providers
        (append ai-mode--models-providers '(ai-mode-openai--get-models)))
  (setq ai-chat--models-providers
        (append ai-chat--models-providers '(ai-mode-openai--get-models))))
```

## Configuration

To enable the OpenAI backend, you need to set your API key in your `.emacs` file:

```elisp
(setq ai-mode-openai--api-key "your-api-key-here")
```

Make sure that your API key is valid and has the necessary permissions for making API requests to ensure seamless interaction.

## Usage

Once configured, the OpenAI backend integrates seamlessly with `ai-mode`, enabling you to use OpenAI models in all `ai-mode` features:

### Model Selection

The backend provides multiple OpenAI models with different configurations:
- **GPT-4o**: Standard and temperature variants (0.1, 1.0)
- **GPT-4o-mini**: Faster, cost-effective option
- **o1 series**: Advanced reasoning models (o1, o1-preview, o1-mini)
- **Experimental models**: o3, o4-mini, GPT-4.1 series

You can switch between models using `ai-mode`'s model selection interface, with each model optimized for different use cases.

### Integration with AI Mode Features

The OpenAI backend works with all `ai-mode` capabilities:
- **Code completion and generation**: Leverage GPT models for intelligent code suggestions
- **Chat interactions**: Use `ai-chat` with OpenAI models for conversational AI assistance
- **Code refactoring**: Apply OpenAI's understanding to improve code structure
- **Documentation generation**: Create comprehensive docs using language models
- **Custom prompts**: Send any prompt through the unified `ai-mode` interface

### Advanced Configuration

Customize model behavior per use case:

```elisp
;; Set custom parameters for specific models
(setq ai-mode-openai--model-temperature 0.2)  ; Lower temperature for more focused responses
(setq ai-mode-openai--default-max-tokens 2000)  ; Increase token limit
(setq ai-mode-openai-request-timeout 120)  ; Extend timeout for complex requests
```

The backend handles all API communication, authentication, and response processing automatically, allowing you to focus on your work while benefiting from OpenAI's powerful language models.

### Adding Custom Models

You can extend the list of available OpenAI models by adding your own custom configurations to `ai-mode`'s model providers. This is useful if you want to test specific model versions, experiment with different `temperature` or `max-tokens` settings, or integrate models not explicitly listed by default.

To add a custom model, modify your Emacs configuration file (e.g., `.emacs` or `init.el`) like this:

```elisp
(add-to-list 'ai-mode--models-providers
             (lambda ()
               (list (ai-mode-openai--make-model "gpt-4o-custom"
                                                       :name "My Custom GPT-4o"
                                                       :temperature 0.5
                                                       :max-tokens 8192))))

;; If you use ai-chat, also add it to ai-chat--models-providers
(add-to-list 'ai-chat--models-providers
             (lambda ()
               (list (ai-mode-openai--make-model "gpt-4o-custom"
                                                       :name "My Custom GPT-4o"
                                                       :temperature 0.5
                                                       :max-tokens 8192))))
```

In this example:
- `gpt-4o-custom` is the model version (which needs to be a valid OpenAI model name).
- `:name` sets a custom display name for your model in `ai-mode`'s selection interface.
- `:temperature` and `:max-tokens` allow you to override the default settings for this specific model.

Remember to restart Emacs or re-evaluate your configuration after making changes. Your custom model will then appear in `ai-mode`'s model selection list.

## Related Resources

### AI Mode Ecosystem

- **[AI Mode](https://github.com/ai-mode/ai-mode)**: The core AI-powered Emacs extension that this backend supports
- **[AI Mode OpenAI](https://github.com/ai-mode/ai-mode-openai)**: OpenAI backend for `ai-mode`.
- **[AI Mode Anthropic](https://github.com/ai-mode/ai-mode-anthropic)**: Anthropic Claude backend for ai-mode
- **[AI Mode DeepSeek](https://github.com/ai-mode/ai-mode-deepseek)**: DeepSeek backend for ai-mode
- **[AI Mode Hugging Face](https://github.com/ai-mode/ai-mode-hf)**: Hugging Face models backend for ai-mode
- **[AI Mode Google Generative AI](https://github.com/ai-mode/ai-mode-google-genai)**: Google Generative AI backend for `ai-mode`.


### Documentation and Community

- **[AI Mode Discussions](https://github.com/ai-mode/ai-mode/discussions)**: Community forum for questions and ideas

## Legal Notice

This project is an independent open-source initiative and is not affiliated with, endorsed by, or sponsored by OpenAI, Inc., Anthropic, PBC, DeepSeek, or Hugging Face, Inc.

OpenAI, GPT, ChatGPT, and related marks are trademarks or registered trademarks of OpenAI, Inc. Claude is a trademark of Anthropic, PBC. DeepSeek is a trademark of DeepSeek. Hugging Face and the Hugging Face logo are trademarks or registered trademarks of Hugging Face, Inc. All other trademarks mentioned in this documentation are the property of their respective owners.

The use of OpenAI's API is subject to OpenAI's terms of service and usage policies. Users are responsible for ensuring their usage complies with all applicable terms and regulations.
