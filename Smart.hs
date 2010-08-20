module Smart where

import ObjC

protocol :: String -> [Protocol] -> [Method] -> Protocol
protocol = Protocol


clazz :: String -> Class -> [Protocol] -> Class
clazz s c p = Class s c p [] (Category "" []) []
