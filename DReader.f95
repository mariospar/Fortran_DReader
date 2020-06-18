 module data_organizer

contains
    subroutine compact(str)

! Converts multiple spaces and tabs to single spaces; deletes control characters;
! removes initial spaces.
    integer :: k,isp,i
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str)):: outstr

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    isp=0
    k=0

    do i=1,lenstr
    ch=str(i:i)
    ich=iachar(ch)

    select case(ich)

    case(9,32)     ! space or tab character
      if(isp==0) then
        k=k+1
        outstr(k:k)=' '
      end if
      isp=1

    case(33:)      ! not a space, quote, or control character
      k=k+1
      outstr(k:k)=ch
      isp=0
    end select

    end do

    str=adjustl(outstr)

    end subroutine compact
    function lowercase(str) result(lcstr)

    ! convert string to lower case

    character (len=*):: str
    character (len=len_trim(str)):: lcstr

    ilen=len_trim(str)
    ioffset=iachar('A')-iachar('a')
    iquote=0
    lcstr=str
    do i=1,ilen
    iav=iachar(str(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then !checks if character is ' or "
    iquote=1
    iqc=iav
    cycle
    end if
    if(iquote==1 .and. iav==iqc) then !checks if previous character was ' or "
    iquote=0
    cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('A') .and. iav <= iachar('Z')) then !checks if character is capital
    lcstr(i:i)=achar(iav-ioffset) !makes it lowercase
    else
    lcstr(i:i)=str(i:i) !if already lowercase
    end if
    end do
    return

    end function lowercase
    subroutine label_finder(line,text,matrix)
        character(30), allocatable :: strarray(:)
        real, allocatable, intent(in) :: matrix(:,:)
        character(*),intent(in) :: line,text
        logical:: flag
        integer :: n,m,i=1,idx
        character(150) :: strmp
        character(1) :: delimiter=' '

        strmp=trim(adjustl(line))

        n=count([(strmp(i:i),i=1,len_trim(strmp))]==delimiter) !counts the number of spaces
        allocate(strarray(n+1))

        m=1
        do i=1,n
            idx=index(strmp(m:),delimiter) !locates the indexes of each space
            strarray(i)=adjustl(strmp(m:m+idx-2)) !appends character to new character array
            m=m+idx
        end do
        strarray(n+1)=adjustl(strmp(m:))
        flag=.true.
        do i=1,n+1
            if (lowercase(trim(strarray(i))) == lowercase(trim(text))) then
                print '(3x,a)', 'Found text!'
                flag=.false.
                call sleep(1)

                call execute_command_line('cls')
                print '(1x,a,a)',trim(strarray(i)),':'
                print '(f8.3)', matrix(:,i)
            end if

        end do

        print *
        if(flag)        print '(3x,a)', 'Text not found! Please check your spelling.'
        if(flag)        print '(3x,a)', 'Here are the labels of the columns from the file you provided.'
        if(flag)        print *
        if(flag)        print '(3x,a)', strmp

        deallocate(strarray)
    end subroutine label_finder
 end module data_organizer
 module column_organizer
    implicit none
contains
    subroutine label_organizer(line,fprint)
        character(150), allocatable :: strarray(:)
        character(1024),intent(in) :: line
        logical, intent(in) :: fprint
        character(150) :: t
        integer :: n,m,p,i=1,idx
        character(150) :: strmp
        character(1) :: delimiter=' '

        strmp=trim(adjustl(line))

        n=count([(strmp(i:i),i=1,len_trim(strmp))]==delimiter) !counts the number of spaces
        allocate(strarray(n+1))

        m=1
        do i=1,n
            idx=index(strmp(m:),delimiter) !locates the indexes of each space
            strarray(i)=adjustl(strmp(m:m+idx-2)) !appends character to new character array
            m=m+idx
        end do
        strarray(n+1)=adjustl(strmp(m:))
        write(t,*) column_counter(line)

        if (fprint) print '(3x,'//trim(adjustl(t))//'a8)', strarray(1:n+1) !dynamically changing array printing format
        deallocate(strarray)
    end subroutine label_organizer
    integer function column_counter(line)
        integer :: i,r
        character(1024), intent(in) :: line
        character, dimension(30) :: m

        do i=1,30
            read (line,*,iostat=r) m(1:i) !when the final column will be reached iostat will be -1
            if (r==-1) exit
        end do
        column_counter=i-1
        !write (*,*) 'The number of columns is:', i-1
    end function column_counter
    subroutine column_indexes(line)
        character(150), allocatable :: strarray(:)
        character(1024),intent(in) :: line
        character(150) :: t,r,v
        integer :: n,m,i=1,idx,j,shift,c
        character(150) :: strmp
        character(1) :: delimiter=' '
        integer, allocatable :: column_range(:)

        strmp=trim(adjustl(line))

        n=count([(strmp(i:i),i=1,len_trim(strmp))]==delimiter) !counts the number of spaces
        allocate(strarray(n+1))
        allocate(column_range(n+1))
        column_range=[(i,i=1,n+1)]
        m=1
        do i=1,n
            idx=index(strmp(m:),delimiter) !locates the indexes of each space
            strarray(i)=adjustl(strmp(m:m+idx-2)) !appends character to new character array
            m=m+idx
        end do
        strarray(n+1)=adjustl(strmp(m:))
        write(t,*) column_counter(line)
        !the construct below is used to dynamically print a character array based on the length of each item
        do c=1,n+1
            if (c==n+1) then
                write (v,*) len_trim(strarray(c))
                write (*,'(2x,'//trim(adjustl(t))//'a'//trim(adjustl(v))//')',advance='yes') strarray(c)
            else
                write (v,*) len_trim(strarray(c))
                write (*,'(2x,'//trim(adjustl(t))//'a'//trim(adjustl(v))//')',advance='no') strarray(c)
            end if
        end do
        !the construct below is used to find the center of each character array item and index it
        !with the number of its column
        !it works well when columns are less than 20
        do j=1,n+1
            if (j==1) then
                shift=nint((len_trim(strarray(j)))/2.)
                write(r,*) shift
                write (*,'('//trim(adjustl(r))//'x,i2)',advance='no') column_range(j)
            else
                if (j>=10) then
                    shift=nint(len_trim(strarray(j))/2.)+1+int(len_trim(strarray(j-1))/2.)
                    write(r,*) shift
                    write (*,'(t'//trim(adjustl(r))//',i0)',advance='no') column_range(j)
                else
                    shift=nint(len_trim(strarray(j))/2.)+2+int(len_trim(strarray(j-1))/2.)
                    write(r,*) shift
                    write (*,'(t'//trim(adjustl(r))//',i0)',advance='no') column_range(j)
                end if
            end if
        end do
        deallocate(column_range)
        deallocate(strarray)
    end subroutine column_indexes
end module column_organizer
 module helloworld

    implicit none
contains
    subroutine welcomer
print *
print *, '.    .                  .    .'
print *, '|\  /|        o         |\  /|'
print *, '| \/ | .-.    .  .--.   | \/ | .-. .--. .  .'
print *, "|    |(   )   |  |  |   |    |(.-' |  | |  |"
print *, "'    ' `-'`--' `-'  `-  '    ' `--''  `-`--`-"

    end subroutine welcomer

    subroutine goodbyer
print *
print *, "         ,--,--'.                     ,_"
print *, '         `- |   |-. ,-. ,-. . , ,-.   |_ ,-. ,-.   . . ,-. . ,-. ,-.'
print *, '          , |   | | ,-| | | |/  `-.   |  | | |     | | `-. | | | | | ,.'
print *, "          `-'   ' ' `-^ ' ' |\  `-'   |  `-' '     `-' `-' ' ' ' `-| `' "
print *, "                            ' `       '                           ,|    "
print *, "                                                                  `'    "
print *, '          ,---.            . .           /\                              '
print *, "          |  -'  ,-. ,-. ,-| |-. . . ,-. \/                              "
print *, "          |  ,-' | | | | | | | | | | |-' ,.                              "
print *, "          `---|  `-' `-' `-' `-' `-| `-' `'                              "
print *, '           ,-.|                   /|                                     '
print *, "           `-+'                  `-'                                     "
print *

    end subroutine goodbyer
 end module helloworld
 module maxmin
    implicit none
contains
    subroutine column_maxmin(col,matrix)
        integer, intent(in) :: col
        real, allocatable,intent(in) :: matrix(:,:)
        integer :: answer

        print '(3x,a)', 'Do you want to find a minimum or a maximum in this column?'
        print *
        print *
        print '(7x,a)', '1) Maximum'
        print '(7x,a)', '2) Minimum'
        print *
        print '(3x,a)', 'Please type the corresponding number...'
        read(*,*) answer
        print *
        select case(answer)
        case(1)
            print '(3x,a)', 'The maximum value in this column is:'
            print '(3x,f8.3)', maxval(matrix(:,col)) !intrinsic function to find max of an array
        case(2)
            print '(3x,a)', 'The minimum value in this column is:'
            print '(3x,f8.3)', minval(matrix(:,col)) !intrinsic function to find min of an array
        end select

    end subroutine column_maxmin
 end module maxmin
 module statistics
    implicit none
 contains
    subroutine stat_elements(matrix,col,results,rows,columns)
        integer, intent(in) :: col,rows,columns
        real, allocatable,intent(inout) :: matrix(:,:)
        real,allocatable :: buf(:)
        real :: sum1,sum2=0,sum3=0,mean,dev
        real, intent(out) :: results(4)
        integer :: irow,krow,mean_item,k

        sum1=0

        allocate(buf(columns))
        !sorts matrix by specific column
        do irow = 1,rows
            krow = minloc( matrix( irow:rows, col ), dim=1 ) + irow - 1

            buf( : )=matrix( irow, : )
            matrix( irow, : ) = matrix( krow, : )
            matrix( krow, : ) = buf( : )
        end do
        call execute_command_line('cls')
        print *
        print '(4x,a)', 'The statistical numbers of this column are:'
        print *
        print '(4x,4(a,3x))', 'Mean Value','Median Value','Variance','Standard Deviation'

        if (mod(rows,2)==0) then  !checks if number is even
            mean_item=rows/2
            sum2=(matrix(mean_item,col) + matrix(mean_item+1,col))/2.
        else
            mean_item=rows/2 +1
            sum2=matrix(mean_item,col)
        end if
        !simple construct to find mean value
        do k=1,rows
            sum1=sum1+matrix(k,col)
        end do
        mean=sum1/rows
        !finds variance
        do k=1,rows
            sum3=(matrix(k,col)-mean)**2
            dev=sum3/rows
        end do


        results(1)=mean
        results(2)=sum2
        results(3)=dev
        results(4)=sqrt(dev) !standard deviation is defined as the square root of variance


        deallocate(buf)
    end subroutine stat_elements
 end module statistics
 module plotter
    use data_organizer
    implicit none
 contains
    subroutine plot(matrix,x,y,columns,rows,labels)
        integer, intent(in) :: x,y,columns,rows
        real, allocatable,intent(inout) :: matrix(:,:)
        character(20), allocatable, intent(in) :: labels(:)
        real,allocatable :: buf(:)
        integer :: size_x,size_y,irow,krow,i
        character(20) :: graph_type,m

        size_x=size(matrix(:,x))
        size_y=size(matrix(:,y))

        print *
        do
        call execute_command_line('cls')
        if (size_x/=size_y) then
            print '(12x,a)', 'Error: Size mismatch '
            exit !error handling
        end if
        print *
        print '(3x,a)', 'You chose to plot '//trim(labels(y))//' with respect to '//trim(labels(x))//''
        print *
        print '(3x,a)', 'Choose the kind of plot you want:'
        print *
        print '(5x,4(a,3x))', 'Bar Graph',' Points Graph','Box Graph','Dots Graph'
        print *
        print *
        read (*,'(a)') m
        graph_type=lowercase(m)
        !checking input
            if (graph_type=='bar') then
                print *
                print '(3x,3a)', 'Entry Accepted ',trim(graph_type),' plot was chosen.'
                call sleep(1)
                exit
            else if(graph_type=='dots') then
                print *
                print '(3x,3a)', 'Entry Accepted ',trim(graph_type),' plot was chosen.'
                call sleep(1)
                exit
            else if(graph_type=='box') then
                print *
                print '(3x,3a)', 'Entry Accepted ',trim(graph_type),' plot was chosen.'
                call sleep(1)
                exit
            else if(graph_type=='points') then
                print *
                print '(3x,3a)', 'Entry Accepted ',trim(graph_type),' plot was chosen.'
                call sleep(1)
                exit
            else
                call execute_command_line('cls')
                print '(3x,a)', 'Mistaken entry. Mind your spelling'
            end if
        end do

        !sorting the matrix by selected column
        allocate(buf(columns))
        do irow = 1,rows
            krow = minloc( matrix( irow:rows, x ), dim=1 ) + irow - 1

            buf( : )=matrix( irow, : )
            matrix( irow, : ) = matrix( krow, : )
            matrix( krow, : ) = buf( : )
        end do
        !write a file with data
        open(1,file='data.dat',status='replace')
            do i=1,size_x
                write(1,*) matrix(i,x),' ',matrix(i,y)
            end do
        close(1)
        !make a gnuplot file with commands based on user's choices
        select case(graph_type)
        case('bar')
            open(2,file='plot.gp',status='replace')
            write(2,*) 'set title "'//trim(labels(y))//'('//trim(labels(x))//')"'
            write(2,*) 'set xlabel "'//trim(labels(x))//'"'
            write(2,*) 'set ylabel "'//trim(labels(y))//'"'
            write(2,*) 'unset key'
            write(2,*) 'set cbtics scale 0'
            write(2,*) 'plot "data.dat" with impulses'
            print *
            write(2,*) "pause -1 '   Hit Enter to Quit' "
            close(2)
        call execute_command_line('gnuplot plot.gp')
        case('dots')
            open(2,file='plot.gp',status='replace')
            write(2,*) 'set title "'//trim(labels(y))//'('//trim(labels(x))//')"'
            write(2,*) 'set xlabel "'//trim(labels(x))//'"'
            write(2,*) 'set ylabel "'//trim(labels(y))//'"'
            write(2,*) 'unset key'
            write(2,*) 'set cbtics scale 0'
            write(2,*) 'plot "data.dat" with dots'
            print *
            write(2,*) "pause -1 '   Hit Enter to Quit' "
            close(2)
        call execute_command_line('gnuplot plot.gp')
        case('box')
            open(2,file='plot.gp',status='replace')
            write(2,*) 'set title "'//trim(labels(y))//'('//trim(labels(x))//')"'
            write(2,*) 'set xlabel "'//trim(labels(x))//'"'
            write(2,*) 'set ylabel "'//trim(labels(y))//'"'
            write(2,*) 'unset key'
            write(2,*) 'set cbtics scale 0'
            write(2,*) 'plot "data.dat" with boxes'
            print *
            write(2,*) "pause -1 '   Hit Enter to Quit' "
            close(2)
        call execute_command_line('gnuplot plot.gp')
        case('points')
            open(2,file='plot.gp',status='replace')
            write(2,*) 'set title "'//trim(labels(y))//'('//trim(labels(x))//')"'
            write(2,*) 'set xlabel "'//trim(labels(x))//'"'
            write(2,*) 'set ylabel "'//trim(labels(y))//'"'
            write(2,*) 'unset key'
            write(2,*) 'set cbtics scale 0'
            write(2,*) 'plot "data.dat" with points '
            print *
            write(2,*) "pause -1 '   Hit Enter to Quit' "
            close(2)
        call execute_command_line('gnuplot plot.gp')
        end select

    end subroutine plot
 end module plotter
 module sorter
    implicit none
 contains
    subroutine sort(matrix,col,columns,rows)
        real, allocatable, intent(inout) :: matrix(:,:)
        integer, intent(in) :: col,columns,rows
        integer :: irow,krow
        real, allocatable :: buf(:)

        allocate(buf(columns))
        do irow = 1,rows
            krow = minloc( matrix( irow:rows, col ), dim=1 ) + irow -1 !intrinsic function to find the location
                                                                        !of the min value of an array
            buf( : )=matrix( irow, : )
            matrix( irow, : ) =matrix( krow, : )
            matrix( krow, : ) = buf( : )
        end do
        deallocate(buf)
    end subroutine
 end module sorter
 program data_reading
    use plotter
    use sorter
    use statistics
    use maxmin
    use helloworld
    use iso_fortran_env
    use data_organizer
    use column_organizer
    implicit none
    integer :: er,rows,m,columns,i,col_sort,d,x,y
    character(30) :: msg,filename,column_name
    character(1024) ::str,col,center,str1,labels,sort_order
    real,allocatable :: numbers(:)
    real, allocatable :: numbers2(:)
    real,allocatable :: numbersmatrix(:,:)
    character(20), allocatable :: labelarray(:)
    real :: results(4)
    logical :: flag=.false.
    call welcomer

    print '(/,/,/8x,a)', 'created by Marios Paraskevas'
    call sleep(2)

    !Main Menu
    call execute_command_line('cls')
    call welcomer
    do

    if (flag)  print '(2x,a)', 'An error has occurred. Please make sure you have spelled the filename correctly.'
    if (flag)  print '(2x,a)', 'Make sure that the input file is not empty.'
    print *
    print '(7x,a)', 'Specify the file you want to open. '
    print '(2x,a)', 'Note: Opening an empty file will result to an error. '
    print *
    print '(a)', 'Last Date Modified           Size     Name'
    print *
    call execute_command_line('dir | findstr "txt csv"') !prints only the files contained in the directory ending with .txt and .csv
    read (*,'(a)') filename
    open(unit=10,file=''//trim(adjustl(lowercase(filename))//''),status='old',iostat=er) !dynamically opens the file chosen
    if (er/=0) then
        flag=.true.
        call execute_command_line('cls')
        cycle
    else
        exit
    end if
    end do
    call execute_command_line('cls')
    call welcomer
    read (10,'(a)') labels
    call compact(labels)
    call label_organizer(labels,.false.)
    columns=column_counter(labels) !finds the numbers of columns
    write(col,*) columns
    rows=1
    rewind(10)
        do
            read(10,'(a)',iostat=er) labelarray
            if (er==iostat_end) exit
            rows=rows+1
        end do

    print *
    print '(3x,a,a)', trim(lowercase(filename)) ,' Opened!'
    call sleep(1) !waits for 1 sec
    call execute_command_line('cls')
    menu: do
            call welcomer
            print *
            print '(7x,a)', 'Choose your option from below.'
            print *
            print '(7x,a)', '1) Preview the file'
            print '(7x,a)', '2) Sort a column'
            print '(7x,a)', '3) Find specific data'
            print '(7x,a)', '4) Find minimum/maximum of a column'
            print '(7x,a)', '5) Statistics'
            print '(7x,a)', '6) Plot a column'
            print '(7x,a)', '7) Exit'
            print *
            write (*,*) 'Type the corresponding number...'
            read (*,*,iostat=d) m
    if (d/=0) then
        print '(3x,a)', 'Warning: Character input is not valid. Try again'
         call sleep(2)
         call execute_command_line('cls')
        cycle
    end if
    select case(m)
    case(1)
    call execute_command_line('cls')
    print '(54x,a)', '---File Preview---'
    print *
    allocate(numbers(columns))
    rewind(10)
        do i=1,rows+1
        if (i==1) then
        read (10,'(a)',iostat=er,iomsg=msg) str
        call compact(str)
        call label_organizer(str,.true.)
        else
            read (10,*,iostat=er,iomsg=msg) numbers
        if (er==iostat_end) then
            write (center,*) (columns*8-len_trim(msg))/2
            print '(3x,'//trim(adjustl(center))//'x,a)', msg
            exit
        else
            print '('//trim(adjustl(col))//'(f8.3))',numbers
            end if
        end if
    end do
        deallocate(numbers)
        print '(3x,a)', 'Press Enter to continue...'
        read (*,*)
        call execute_command_line('cls')
    case(7)
        call execute_command_line('cls')
        call goodbyer
        exit
    case(2)
        call execute_command_line('cls')
        rewind(10)
        read(10,'(a)') str1
        call compact(str1)
        do
        print *
        print '(3x,a)', 'Select a column to sort:'
        print *
        write (col,*) column_counter(str1)
        call column_indexes(str1)
        print *
        print *
        print '(3x,a)', 'Please enter the column index...'
        read (*,*,iostat=d) col_sort
        if (d/=0) then
            call execute_command_line('cls')
            print '(3x,a)', 'Only integer input is allowed. Try again'
            cycle
        else
            exit
        end if
        end do
        allocate(numbers2(columns))
        if(allocated(numbersmatrix)) deallocate(numbersmatrix)
        allocate(numbersmatrix(rows-1,columns))


        do i=1,rows-1
                read(10,*,iostat=er) numbers2
                numbersmatrix(i,:)=numbers2(:)
        end do
        deallocate(numbers2)

        call sort(numbersmatrix,col_sort,columns,rows)

        do
        print '(3x,a)', 'Sort from highest value to lowest?   Yes or No?'
        read (*,*,iostat=er) sort_order
        if (lowercase(sort_order)=='no') then
            call execute_command_line('cls')
            call label_organizer(str1,.true.)
            do i=1,rows-1
            print '(16f8.3)', numbersmatrix(i,:)
            end do
            exit
        else if (lowercase(sort_order)=='yes') then
            call execute_command_line('cls')
            call label_organizer(str1,.true.)
            do i=rows-1,1,-1
            print '(16f8.3)', numbersmatrix(i,:)
            end do
            exit
        else
             print '(3x,a)', 'Wrong input. Try again'
             cycle
        end if
        end do
        deallocate(numbersmatrix)
        print *
        print '(3x,a)', 'Press Enter to continue...'
        read(*,*)
        call execute_command_line('cls')
    case(3)
        call execute_command_line('cls')
        rewind(10)

        read(10,'(a)') str1
        call compact(str1)
        print *
        print '(3x,a)', trim(str1)
        print *
        print *
        print '(3x,a)', 'Please type the name of the column you want to preview:'
        print *
        read(*,*) column_name

        allocate(numbers2(columns))
        if (allocated(numbersmatrix)) deallocate(numbersmatrix)
        allocate(numbersmatrix(rows-1,columns))


        do i=1,rows-1
                read(10,*,iostat=er) numbers2
                numbersmatrix(i,:)=numbers2(:)
        end do
        deallocate(numbers2)
        call label_finder(str1,column_name,numbersmatrix)
        print *
        print '(3x,a)', 'Press Enter to continue...'
        read(*,*)
        call execute_command_line('cls')
    case(4)
        rewind(10)
        call execute_command_line('cls')

        read(10,'(a)') str1
        call compact(str1)
        do
        print *
        print '(3x,a)', 'Select a column to search through:'
        print *
        call column_indexes(str1)
        print *
        print *
        print '(3x,a)', 'Please enter the column index...'
        read (*,*,iostat=d) col_sort
        if (d/=0) then
            call execute_command_line('cls')
            print '(3x,a)', 'Only integer input is allowed. Try again'
            cycle
        else
            exit
        end if
        end do
        allocate(numbers2(columns))
        if(allocated(numbersmatrix)) deallocate(numbersmatrix)
        allocate(numbersmatrix(rows-1,columns))


        do i=1,rows-1
                read(10,*,iostat=er) numbers2
                numbersmatrix(i,:)=numbers2(:)
        end do
        deallocate(numbers2)

        call column_maxmin(col_sort,numbersmatrix)
        deallocate(numbersmatrix)
        print *
        print '(3x,a)', 'Press Enter to continue...'
        read(*,*)
        call execute_command_line('cls')
    case(5)
        rewind(10)
        call execute_command_line('cls')

        read(10,'(a)') str1
        call compact(str1)
        do
        print *
        print '(3x,a)', 'Select a column to work with:'
        print *
        call column_indexes(str1)
        print *
        print *
        write (*,'(3x,a)',advance='no') 'Please enter the column index...'
        read (*,*,iostat=d) col_sort
        if (d/=0) then
            call execute_command_line('cls')
            print '(3x,a)', 'Only integer input is allowed. Try again'
            cycle
        else
            exit
        end if
        end do
        allocate(numbers2(columns))
        if (allocated(numbersmatrix)) deallocate(numbersmatrix)
        allocate(numbersmatrix(rows-1,columns))


        do i=1,rows-1
                read(10,*,iostat=er) numbers2
                numbersmatrix(i,:)=numbers2(:)
        end do
        deallocate(numbers2)

        call stat_elements(numbersmatrix,col_sort,results,rows,columns)
        print '(1x,4(f11.3,2x))', results
        deallocate(numbersmatrix)
        print *
        print '(3x,a)', 'Press Enter to continue...'
        read(*,*)
        call execute_command_line('cls')
    case(6)
        call execute_command_line('cls')
        rewind(10)
        read(10,'(a)') str1
        call compact(str1)
        do
        print '(55x,a)', '---Plotter Section---'
        print '(3x,a)', 'Please choose the columns you want to plot'
        print *
        print '(3x,a)', "The first number you'll type will be the x-axis and the second will be the y-axis"
        print *
        call column_indexes(str1)
        print*
        read (*,*,iostat=d) x,y
        if (d/=0) then
            call execute_command_line('cls')
            print '(3x,a)', 'Only integer input is allowed. Try again'
            cycle
        else
            exit
        end if
        end do
        rewind(10)
        allocate(labelarray(columns))
        read(10,*) labelarray(:)
        allocate(numbers2(columns))
        if(allocated(numbersmatrix)) deallocate(numbersmatrix)
        allocate(numbersmatrix(rows-1,columns))


        do i=1,rows-1
                read(10,*,iostat=er) numbers2
                numbersmatrix(i,:)=numbers2(:)
        end do
        deallocate(numbers2)

        call plot(numbersmatrix,x,y,columns,rows,labelarray)
        deallocate(numbersmatrix)
        deallocate(labelarray)
        print *
        call execute_command_line('cls')
    case default
        print '(3x,a)', 'This option does not exist. Please try again.'
        call sleep(2)
        call execute_command_line('cls')
    end select
    end do menu
    close(10)
end program data_reading
