# Fortran_DReader

## About  
`DReader` is a fast, user friendly and simple data analyzer coded in Fortran, running in Windows. It can sort data, find statistical values as well as plot variables in 2D space, using gnuplot. DReader can read
**.txt** and **.csv** file formats, in which their first row includes the labels, or "features".  

## Installation    

Unfortunately, DReader is a dedicated to Windows application, but it can be modified in order to run on other OS. *(More about that later)* 


### Compilers  

There are many fortran compilers but it seems that the most common one is **`gfortran`**, which is part of the `MINGW-W64 GCC Compiler`.   
To install it, follow this [link](http://mingw-w64.org/doku.php/download)  
#### **Note:**  
*Make sure to add the mingw-w64 binaries to your environment variables*

### Prerequisites
* ```gnuplot```

You can install gnuplot from this [link](https://sourceforge.net/p/gnuplot/gnuplot-main/ci/master/tree/) 

## Getting Started  

To compile the DReader.f95 file to an executable, type:  
1. Open Windows Terminal
2. Change directory to DReader.f95's parent directory
3. Type:  
~~~ 
gfortran DReader.f95 -o [your_prefered_name].exe 
~~~
4. And finally run the executable  
~~~
[your_prefered_name].exe
~~~

## Contributors

This project was part of my semester evaluation towards the *Programming in Fortran* course I attended during my Sophomore year on Chemical Engineering. 
This project welcomes contributions and suggestions.

## License

The DReader is Copyright (c) 2020 Marios Paraskevas, under the MIT License. See [LICENSE](https://github.com/mariospar/Fortran_DReader/blob/master/LICENSE) for details.
