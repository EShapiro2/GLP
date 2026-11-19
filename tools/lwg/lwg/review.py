"""Review: every open choice is a question; a person answers (answers.json). Unanswered questions stay open and Build uses the default shown."""
import json
def run(rows, answers_path):
    answers = json.load(open(answers_path, encoding='utf8'))
    qs = []
    for r in rows:
        if r.get('question'):
            r['answer'] = answers.get(r['id'], '')
            qs.append(r)
    return qs
def markdown(qs, title):
    out = ['# Questions: %s' % title, '']
    for i, r in enumerate(qs, 1):
        out.append('%d. (provision %s) %s%s' % (i, r['id'], r['question'], '  **Answer:** ' + r['answer'] if r['answer'] else '  *not answered; default used*'))
    return '\n'.join(out) + '\n'
