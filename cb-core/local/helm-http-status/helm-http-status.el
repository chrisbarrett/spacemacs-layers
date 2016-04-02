;;; helm-http-status.el --- Helm source for HTTP status codes  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((helm "20160401.1145"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(autoload 'helm-other-buffer "helm")

(defun helm-http-status ()
  "Helm command to display HTTP status codes."
  (interactive)
  (let ((source '((name . "HTTP STATUS")
                  (candidates . (("100 Continue") ("101 Switching Protocols")
                                 ("102 Processing") ("200 OK")
                                 ("201 Created") ("202 Accepted")
                                 ("203 Non-Authoritative Information") ("204 No Content")
                                 ("205 Reset Content") ("206 Partial Content")
                                 ("207 Multi-Status") ("208 Already Reported")
                                 ("300 Multiple Choices") ("301 Moved Permanently")
                                 ("302 Found") ("303 See Other")
                                 ("304 Not Modified") ("305 Use Proxy")
                                 ("307 Temporary Redirect") ("400 Bad Request")
                                 ("401 Unauthorized") ("402 Payment Required")
                                 ("403 Forbidden") ("404 Not Found")
                                 ("405 Method Not Allowed") ("406 Not Acceptable")
                                 ("407 Proxy Authentication Required") ("408 Request Timeout")
                                 ("409 Conflict") ("410 Gone")
                                 ("411 Length Required") ("412 Precondition Failed")
                                 ("413 Request Entity Too Large")
                                 ("414 Request-URI Too Large")
                                 ("415 Unsupported Media Type")
                                 ("416 Request Range Not Satisfiable")
                                 ("417 Expectation Failed") ("418 I'm a teapot")
                                 ("422 Unprocessable Entity") ("423 Locked")
                                 ("424 Failed Dependency") ("425 No code")
                                 ("426 Upgrade Required") ("428 Precondition Required")
                                 ("429 Too Many Requests")
                                 ("431 Request Header Fields Too Large")
                                 ("449 Retry with") ("500 Internal Server Error")
                                 ("501 Not Implemented") ("502 Bad Gateway")
                                 ("503 Service Unavailable") ("504 Gateway Timeout")
                                 ("505 HTTP Version Not Supported")
                                 ("506 Variant Also Negotiates")
                                 ("507 Insufficient Storage") ("509 Bandwidth Limit Exceeded")
                                 ("510 Not Extended")
                                 ("511 Network Authentication Required")))
                  (action . message))))
    (helm-other-buffer (list source) "*helm http-status*")))

(provide 'helm-http-status)

;;; helm-http-status.el ends here
