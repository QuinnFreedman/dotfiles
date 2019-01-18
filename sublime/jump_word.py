import sublime
import sublime_plugin
import itertools

highlight = "invalid"


class JumpWordCommand(sublime_plugin.WindowCommand):
    def run(self):
        active_view = self.window.active_view()
        regions = [
            sublime.Region(0, 2),
            sublime.Region(10, 20)
        ]
        active_view.add_regions(
            "jump_match_regions", regions, JUMP_TARGET_SCOPE)


def get_labels():
    chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for first in [""] + list(chars):
        for second in chars:
            yield (first + second)


class AddJumpLabelsCommand(sublime_plugin.TextCommand):
    """Command for adding labels to the views"""

    def run(self, edit):
        region = self.view.visible_region()
        next_search = region.begin()
        last_search = region.end()
        words = []

        while next_search < last_search:
            word = self.view.find(r"\w+", next_search)

            if not word or word.end() > last_search:
                break

            next_search = word.end()
            words.append(word)

        labels = list(itertools.islice(get_labels(), len(words)))
        regions = [sublime.Region(w.begin(), w.begin() + len(label))
                   for (w, label) in zip(words, labels)]

        self.view.add_regions("jump_hints", regions, highlight)
        for region, label in zip(regions, labels):
            self.view.replace(edit, region, label)


class RemoveJumpLabelsCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        self.view.erase_regions("jump_hints")
        self.view.end_edit(edit)
        self.view.run_command("undo")
