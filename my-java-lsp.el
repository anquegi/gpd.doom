;;; my-javalsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (let ((lombok-jar-path (expand-file-name "lombok.jar" doom-user-dir)))
    (setq lsp-java-vmargs (list "-Dfile.encoding=utf8"
                                "-server"
                                "-Xms4G"
                                "-Xmx4G"
                                "-Xmn2G"
                                "-Xss512K"
                                "-XX:MetaspaceSize=1536M"
                                "-XX:MaxMetaspaceSize=1536M"
                                "-XX:InitialCodeCacheSize=128M"
                                "-XX:ReservedCodeCacheSize=512M"
                                "-XX:+UseG1GC"
                                "-XX:+UseStringDeduplication"
                                "-XX:GCTimeRatio=19"
                                "-XX:AdaptiveSizePolicyWeight=90"
                                "-Dsun.zip.disableMemoryMapping=true"
                                (concat "-Xbootclasspath/a:" lombok-jar-path)
                                (concat "-javaagent:" lombok-jar-path))))

  (setq lsp-java-configuration-runtimes (cond (IS-LINUX '[(:name "JavaSE-11"
                                                           :path "/usr/lib/jvm/java-19-openjdk"
                                                           :default t)
                                                          ])
                                              (t nil)))

  (setq lsp-java-java-path "/usr/lib/jvm/java-19-openjdk/bin/java")

  (setq lsp-java-import-gradle-enabled nil
        lsp-java-format-enabled nil
        lsp-java-format-comments-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-max-concurrent-builds 5
        lsp-java-completion-max-results 30
        lsp-java-folding-range-enabled nil
        lsp-java-signature-help-enabled nil
        lsp-java-selection-enabled nil
        lsp-java-trace-server "messages"
        lsp-java-maven-download-sources t
        lsp-java-sources-organize-imports-star-threshold 5
        lsp-java-sources-organize-imports-static-star-threshold 3
        ;; Support java decompiler
        lsp-java-content-provider-preferred "fernflower")

  (setq lsp-java-completion-favorite-static-members ["org.junit.Assert.*" "org.junit.Assume.*" "org.junit.jupiter.api.Assertions.*" "org.junit.jupiter.api.Assumptions.*" "org.junit.jupiter.api.DynamicContainer.*" "org.junit.jupiter.api.DynamicTest.*" "org.mockito.Mockito.*" "org.mockito.ArgumentMatchers.*" "org.mockito.Answers.*" "org.hamcrest.MatcherAssert.*" "org.hamcrest.Matchers.*"])

  (if (and (modulep! :editor format)
           (modulep! +google-java-format))
      (progn
        (set-formatter! 'google-java-format
          '("google-java-format" "-" "-a" "-" "--skip-sorting-imports")
          :modes 'java-mode)

        (setq-hook! 'java-mode-hook
          tab-width 4
          fill-column 120))
    (setq lsp-java-format-enabled t
          lsp-java-format-comments-enabled t))

  )

(after! dap-mode
  (setq dap-java-test-additional-args '("-n" "\".*(Test|IT|Tests).*\""))
  (setq dap-java-java-command "/usr/lib/jvm/java-19-openjdk/bin/java")
  (setq inhibit-eol-conversion t))
