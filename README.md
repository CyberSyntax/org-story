# Org-story

Org-story is an Emacs package that extracts narrative elements (such as characters, locations, and scenes) from an Org-mode file by detecting Org ID links. It then filters and cleans corresponding headings to build a single-paragraph “narrative plot” view—making it easier for writers to overview and navigate their story structure.

## Overview

Many writers use Org-mode to structure their narrative projects. With Org-story, you can:

• Extract narrative element references (via “id:” links) from the current Org buffer.  
• Cache these IDs so that extraction happens at most once per day (improving performance).  
• Interactively choose one or more narrative elements to filter headings that mention them.  
• Clean up heading text by removing extraneous markers (e.g., asterisks, priority markers, SCHEDULED lines, and inline link syntax) and normalize spacing.  
• Display the concatenated narrative plot in a separate buffer with a clickable interface that lets you jump back to the original heading.

When you run the narrative plot command, Org-story automatically splits the window vertically. The upper window shows the original Org file (approximately 38.2% of the height, following the golden ratio) while the lower window displays the narrative plot (about 61.8% of the height). In the plot window, pressing RET or clicking a segment takes you directly to its source location in the Org file.

## Features

- **Narrative Element Extraction:** Scans the current buffer for Org “id:” links and retrieves titles from the Org-roam database.
- **Caching:** Saves the extracted ID links to a cache file (refreshing only once per day) when the Org file is saved to disk.
- **Interactive Selection:** Prompts you to select narrative elements interactively. You can reuse your last selection or add new elements.
- **Filtered & Cleaned View:** Matches headings that contain the selected narrative elements, cleans up the text (removing extraneous syntactic markers), and concatenates them into one neatly formatted paragraph.
- **Clickable Navigation:** Associates each narrative segment with its original location so that clicking or pressing RET opens the source heading.
- **Window Layout:** Splits your window using the golden ratio, showing the source buffer above and the narrative plot below.

## Requirements

- [Org-mode](https://orgmode.org/) (comes with Emacs)
- [org-element](https://orgmode.org/manual/Org_002delement-API.html)
- [org-roam](https://github.com/org-roam/org-roam) (for retrieving titles from Org IDs)
- `cl-lib` and `seq` (available in current Emacs versions)

## Installation

1. Clone or download the Org-story repository.
2. Add the directory to your Emacs `load-path`. For example:

   (add-to-list 'load-path "/path/to/Org-story-directory")

3. Require the package in your Emacs configuration:

   (require 'Org-story)

4. Org-story binds the interactive command to `C-c s p` by default. Restart Emacs or load your configuration to activate the package.

## Usage

1. Open an Org-mode file that contains narrative elements via Org ID links.
2. Press `C-c s p` to invoke the interactive narrative plot command (`Org-story-show-plot`).
3. Follow the on-screen prompts:
   - When first prompted, you can type a narrative element name (from the available titles) or simply press RET to reuse your previous selection.
   - Continue to add narrative elements until you are finished.
   - If more than one narrative element is selected, you will be asked whether to combine them with an “and” or “or” operator.
4. The source buffer is split vertically:
   - The upper window shows the original Org file.
   - The lower window displays the narrative plot in the `*Narrative Plot*` buffer.
5. In the narrative plot buffer, clicking on a segment or pressing RET on it jumps back to the corresponding heading in the source file.

## Customization

- **Cache Directory:**  
  By default, Org-story stores its cache in a subdirectory of your `org-roam-directory` or within your `user-emacs-directory`. You can customize the variable `org-roam-id-links-cache-directory` to change this location.

- **Window Layout and Appearance:**  
  The plot view is displayed using a golden-ratio based split. Feel free to adjust your window-management settings if desired.

## Troubleshooting

- **No Narrative Elements Found:**  
  Ensure your Org file contains valid “id:” links and that the corresponding IDs exist in the Org-roam database.
  
- **Stale Cache:**  
  The cache is refreshed at most once per day. If you expect changes, either wait for the next day or manually delete the cache file.

## Key Bindings

- `C-c s p`: Invoke the narrative plot view command.
- In the narrative plot (`*Narrative Plot*`) buffer:
  - `RET` or left-click on a segment: Jump back to the original heading in the source buffer.

## Contributing

Contributions, bug reports, and feature requests are welcome! Feel free to fork the repository, submit pull requests, or open issues on GitHub.

## License

`Org-story` is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.  
Refer to the [LICENSE](./LICENSE) file for detailed terms and conditions.
