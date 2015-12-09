import pprint
import sys


class TraceCalls(object):
    def __init__(self, stream=sys.stdout, indent='  ', files=(),
                 print_locals=True):
        self.stream = stream
        self.indent = indent
        self.files = frozenset(files)
        self.stack_depth = 0
        self.max_depth = 0
        self.print_locals = print_locals
    def __call__(self, frame, event, arg):
        if event == 'call':
            if not self.files or frame.f_code.co_filename in self.files:
                self.stack_depth += 1
                if self.stack_depth > self.max_depth:
                    self.max_depth = self.stack_depth
                name = frame.f_code.co_name
                print(self.indent * self.stack_depth, frame.f_code.co_filename, name)
                if self.print_locals and not (name == '__str__' or name == '__init__' or name == '<lambda>'):
                    pprint.pprint(frame.f_locals)
        elif event == 'return':
            if not self.files or frame.f_code.co_filename in self.files:
                print(self.indent * self.stack_depth, frame.f_code.co_filename, frame.f_code.co_name, arg)
                self.stack_depth -= 1
        return self


class StackDepth(object):
    def __init__(self):
        self.stack_depth = 0
        self.max_depth = 0
    def __call__(self, frame, event, arg):
        if event == 'call':
            self.stack_depth += 1
        if event == 'return':
            self.stack_depth -= 1
        return self
