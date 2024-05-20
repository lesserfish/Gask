function! GetVisualSelection()
    " Why is this not a built-in Vim script function?!
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfunction


function! UpdateSelection(content) range
    let [lineStart, columnStart] = getpos("'<")[1:2]
    execute "normal! gv\"xd"

    let lineContent = getline(lineStart)
    let beforeSegment = strpart(lineContent, 0, columnStart - 1)
    let afterSegment = strpart(lineContent, columnStart - 1)

    call deletebufline('', lineStart)
    let segments = split(a:content, "\n")
    let segments[0] = beforeSegment . segments[0]
    let segments[-1] = segments[-1] . afterSegment 

    for segment in reverse(segments)
        call append(lineStart - 1, segment)
    endfor
endfunction

function! Gask(user_request) range
    let selection = GetVisualSelection()
    let cmd = "gask -q"
    let raw_prompt = "You will be provided with a segment of text and a specific request. Your task is to modify the text in accordance with the request. Ensure that all necessary modifications are made to fulfill the request. Once completed, output the updated text.\n\nRequest: " . a:user_request . "\n\nText:" .  selection
    let prompt = shellescape(raw_prompt)
    let request = cmd . " " . prompt
    let response = system(request)
    if v:shell_error
        echoerr "Error running gask :/"
        return
    endif
    call UpdateSelection(response)
endfunction

