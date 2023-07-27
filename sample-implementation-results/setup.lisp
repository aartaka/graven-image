(require 'asdf)
(progn
  (push #p"~/git/trivial-gray-streams/" asdf:*central-registry*)
  (push #p"~/git/closer-mop/" asdf:*central-registry*)
  (push #p"~/git/graven-image/" asdf:*central-registry*))
(asdf:load-system :graven-image)
