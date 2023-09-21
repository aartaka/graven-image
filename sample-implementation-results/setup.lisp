(require 'asdf)
(progn
  (push #p"~/git/closer-mop/" asdf:*central-registry*)
  (push #p"~/git/graven-image/" asdf:*central-registry*))
(asdf:load-system :graven-image)
(use-package :graven-image)
