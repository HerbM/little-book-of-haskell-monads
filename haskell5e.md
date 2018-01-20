## 5E. Recursion and Monads

**Example**    
Below shows a program that uses IO to read a file,
which which may have *long lines*, lines that are longer than 3 chars.
We'll count the number of long lines and log the number using `WriterT`.

Let's say for example, we have the file `foo.txt` 
with the following contents:
```
AAA
BBBBBB
CC
DDDDDDDDDD
E
```

Then we would expect it to say `2` because there are *two* lines that have a length
of more than 3 columns (namely the line with `B`'s, and the line with `D`'s).   
Here is our program:

```haskell
import Control.Monad.Writer (WriterT, tell, lift, runWriterT)

-- [1]: Break the file down into lines count how many are too long
-- [2]: Here we are using WriterT monad as usual
-- [3]: Here we are accessing inner monad, the IO monad, then lifting up to WriterT
shortLinesCount :: String -> Int
shortLinesCount = length . shortOnly . lines -- [1]
    where shortOnly = filter ((> 3) . length)

checkLongLines :: WriterT [(String, String)] IO ()
checkLongLines = do -- WriterT monad
    tell [("foo.txt", "Attempting to Read File")] -- [2]
    foo <- lift . readFile $ "foo.txt"            -- [3]
    let count = shortLinesCount foo
    tell [("foo.txt", "Number of long lines: " ++ show count)] -- [2]

main = do
    runWriterT checkLongLines
```

*Output*    
```
((),[("foo.txt","Attempting to Read File"),("foo.txt","Number of long lines: 2")])
```


----

**Example**    
Here's a great example is that taken from RWH (Chapter 18).
In this example, we'll write a function `countDir` that 
uses the `IO` monad to count how many files are in a directory
and then use `WriterT` to incrementally write the result to log.

```haskell
import CountEntries (listDirectory)
import Control.Monad.Writer (WriterT, tell, lift)

countDir :: FilePath -> WriterT [(FilePath, Int)] IO ()
countDir path = do
  dir <- lift . listDirectory $ path
  tell [(path, length dir)]
  
main = do
  print $ runWriterT countDir
```


----

**Example**    
We'll now extend our previous example
so that we can make this count directives *recursively*.    
Let's first setup a new directory structure.

```
$ mkdir -p example/{A,B,C}
$ touch example/A/a{1,2,3}.txt
$ touch example/B/b{1,2,3}.txt
$ touch example/C/c{1,2,3}.txt
```

This creates the following directory structure:
```
$ tree .
├── example
│   ├── A
│   │   ├── a1.txt
│   │   ├── a2.txt
│   │   └── a3.txt
│   ├── B
│   │   ├── b1.txt
│   │   ├── b2.txt
│   │   └── b3.txt
│   └── C
│       ├── c1.txt
│       ├── c2.txt
│       └── c3.txt
└── test.hs
```

```haskell
import Control.Monad (forM_, when, liftM)
import System.FilePath ((</>))
import Control.Monad.Writer (WriterT, tell, lift, runWriterT)
import System.Directory (doesDirectoryExist, getDirectoryContents)

--- [1]: access the inner IO monad and then lift it to WriterT
--- [2]: use WriterT to write the result to the log
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do -- WriterT monad
  -- count current directory
  currentdir <- lift . listDirectory $ path -- [1]
  tell [(path, length currentdir)]          -- [2]

  -- recursively count subdirectories
  forM_ currentdir $ \directory -> do -- WriterT monad
    let fullpath = path </> directory
    isDir <- lift . doesDirectoryExist $ fullpath -- [1]
    when isDir $ countEntries fullpath

main = do
  runWriterT (countEntries "example")
```

*Output*    
```
((),[("example",3),("example/A",3),("example/B",3),("example/C",3)])
```

----




----

**Footnotes**    

[1] :you may come across the following compilation
error:
```
Module ‘System.Directory’ does not export ‘listDirectory’
```

Note that the above relies on `listDirectory` function.
which was recently added (added in v1.2.5.0 of the `directory` package) 
To check your version of the `directory` package, run:
```
ghc-pkg list | grep directory
```

To make the code compile, you can use:
```haskell
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."
```
