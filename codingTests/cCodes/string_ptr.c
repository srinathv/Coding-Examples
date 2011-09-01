 
#include <stdio.h>    /* stdin, printf, and fgets */
#include <string.h>   /* for all the new-fangled string functions */

/* this function is designed to remove the newline from the end of a string
entered using fgets.  Note that since we make this into its own function, we
could easily choose a better technique for removing the newline.  Aren't
functions great? */
void strip_newline( char *str, int size )
{
    int i;

    /* remove the null terminator */
    for (  i = 0; i < size; ++i )
    {
        if ( str[i] == '\n' )
        {
            str[i] = '\0';

            /* we're done, so just exit the function by returning */
            return;   
        }
    }
    /* if we get all the way to here, there must not have been a newline! */
}

int main()
{
    char name[50];
    char lastname[50];
    char fullname[100]; /* Big enough to hold both name and lastname */

    printf( "Please enter your name: " );
    fgets( name, 50, stdin );

    /* see definition above */
    strip_newline( name, 50 );

    /* strcmp returns zero when the two strings are equal */
    if ( strcmp ( name, "Alex" ) == 0 ) 
    {
        printf( "That's my name too.\n" );
    }
    else                                     
    {
        printf( "That's not my name.\n" );
    }
    // Find the length of your name
    printf( "Your name is %d letters long",  (unsigned int)strlen ( name), ".\n" );
    printf( "Enter your last name: " );
    fgets( lastname, 50, stdin );
    strip_newline( lastname, 50 );
    fullname[0] = '\0';            
    /* strcat will look for the \0 and add the second string starting at
       that location */
    strcat( fullname, name );     /* Copy name into full name */
    strcat( fullname, " " );      /* Separate the names by a space */
    strcat( fullname, lastname ); /* Copy lastname onto the end of fullname */
    printf( "Your full name is %s\n",fullname );

    getchar();

    return 0;
}
