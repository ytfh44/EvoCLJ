with open('src/evoclj/context/compressor.clj', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace both occurrences of (read-string raw-response) with (clojure.edn/read-string raw-response)
content = content.replace('(read-string raw-response)', '(clojure.edn/read-string raw-response)')

with open('src/evoclj/context/compressor.clj', 'w', encoding='utf-8') as f:
    f.write(content)
print('Replaced read-string with clojure.edn/read-string in compressor.clj')
