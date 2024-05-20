# Gask

https://github.com/lesserfish/gask/assets/73536889/d2dd37a2-cb52-462c-bccb-3fb2b5659f95

## Gask

Gask is a tool that lets you communicate with Gemini via the terminal. Usage is as follows:


    Gask
    
    Usage: gask [-c|--config CONFIG_FILE] [-k|--key KEY] [-m|--model MODEL] 
                [-i|--interactive] [-e|--eofMode] [-q|--quiet] 
                [--prompt-color COLOR] [--user-color COLOR] [--model-color COLOR] 
                [-p|--prompt PROMPT_MESSAGE] [INPUT]
    
      Frontend for Gemini
    
    Available options:
      -c,--config CONFIG_FILE  Configuration file path
      -k,--key KEY             API key
      -m,--model MODEL         Model
      -i,--interactive         Interactive mode
      -e,--eofMode             Wait for EOF
      -q,--quiet               Don't format text or output terminal prompts
      --prompt-color COLOR     Color for the terminal prompt
      --user-color COLOR       Color for the user text
      --model-color COLOR      Color for the model text
      -p,--prompt PROMPT_MESSAGE
                               Starting prompt for the model
      -h,--help                Show this help text
## Library

We also include a library that allows you to use Gemini API from Haskell. Documentation is currently a work in progress.

## Warning

Be carefull simply piping the output of Gemini into your shell.

It does not always have your best interests in mind:

![image](https://github.com/lesserfish/gask/assets/73536889/8dc1ccfd-4d7d-43ba-97de-ca96831967f3)

You can try setting up an alias to gask with a reasonable (safe) prompt, or just write a function that asks for confirmation before evaluating the output of gask.

