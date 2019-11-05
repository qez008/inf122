import System.Directory

flyttLn fn =
  if (doesFileExist fn) then 
    do
      fh <- openFile fn ReadMode
      content <- hGetContents fh
      let (l1:rest) = lines content
          newContent = rest ++ [l1]
      (fn2, fh2) <- openTempFile "." "tempt"
      hPutStr fh2 newContent
      hClose fh
      hClose fh2
      removeFile fn
      renameFile fn2 fn
  else
    return ()