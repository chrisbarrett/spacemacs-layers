;;; Compiled snippets and support files for `haskell-cabal-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-cabal-mode
                     '(("exe" "executable ${1:name}\n  -- .hs or .lhs file containing the Main module.\n  main-is:             Main.hs\n\n  -- Modules included in this executable, other than Main.\n  -- other-modules:\n\n  -- LANGUAGE extensions used by modules in this package.\n  -- other-extensions:\n\n  -- Other library packages from which modules are imported.\n  build-depends:       base >=4.7 && <4.8\n                     , classy-prelude >=0.9.5 && <0.9.6\n                     , cmdargs >=0.10.9 && <0.10.10\n                     , configurator >= 0.3.0 && <0.3.1\n                     , transformers >= 0.4.1.0\n\n  -- Directories containing source files.\n  hs-source-dirs:      src\n\n  -- Base language which the package is written in.\n  default-language:    Haskell2010" "executable"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)
                       ("lib" "library\n  hs-source-dirs:      src\n  exposed-modules:     $0\n  build-depends:       base >=4.7 && <4.8\n  default-language:    Haskell2010" "library"
                        (yas/bol\?)
                        nil nil nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Nov 29 10:54:35 2014
