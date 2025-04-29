# sam.el

**sam.el** is a structural editing package for Emacs, inspired by Rob
Pike's `sam` editor from Plan 9.

It lets you select text regions based on regular expressions (or
existing selections like `iedit` or a region) and perform, composable
structural edits with minimal ceremony.

<img src="https://github.com/hkjels/sam.el/blob/main/sam.png?raw=true" alt="Sam & max + Mr. Emacs" width="342" />


---

## Installation

Clone the repository and add it to your Emacs `load-path`:

```elisp
(add-to-list 'load-path "/path/to/sam.el/")
(require 'sam)
```

---

## Usage

1. `M-x sam`
2. If you have a region active or `iedit-mode` running, `sam` will automatically use those matches. Otherwise, it will prompt you for a regex.
3. A transient menu appears, offering actions:
   - `a`: Append text after matches
   - `c`: Replace entire matches
   - `d`: Delete matches
   - `i`: Insert text before matches
   - `s`: Substitute inside matches
   - `w`: Wrap matches with template
   - `m`: Move matches (to beginning, end, or another buffer)
   - `t`: Copy matches (to beginning, end, or another buffer)
   - `|`: Pipe matches through a shell command
   - `r`: Refine matches with a new regex
   - `q`: Quit structural editing
4. Actions can be chained: refine → edit → refine → move → etc.

---

## Example Workflows

- **Extract all TODO comments into a new buffer**:
  1. `M-x sam`
  2. Regex: `^.*TODO.*$`
  3. `t` → `new-buffer`

- **Refactor all error lines and reformat them**:
  1. `M-x sam`
  2. Regex: `^ERROR:.*`
  3. `|` → `sed 's/ERROR:/[ERROR]/'`

- **Enumerate bullet points with numbering**:

Say you’ve written a list like this:

```
- Install dependencies
- Run the tests
- Deploy to production
```

…and you want to number them:

1. `M-x sam`  
2. Regex: `^- .+`  
3. `w` → Template: `{index}. {match}`

You get:

```
1. - Install dependencies
2. - Run the tests
3. - Deploy to production
```
