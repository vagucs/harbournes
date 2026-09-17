/*
  Name: mysql.cpp
  Copyright: 2005 Frantisek Sidak
  Author: Frantisek Sidak
  Date: 23.11.2005 13:00
  Description: simple MySQL connection test
*/


#include <windows.h>
#include <iostream>
#include <mysql/mysql.h>

using namespace std;

int main()
{
    //connection params
    char *host = "localhost";
    char *user = "";
    char *pass = "";
    char *db = "";

    //sock
    MYSQL *sock;
    sock = mysql_init(0);
    if (sock) cout << "sock handle ok!" << endl;
    else {
         cout << "sock handle failed!" << mysql_error(sock) << endl;
    }

    //connection
    if (mysql_real_connect(sock, host, user, pass, db, 0, NULL, 0))
         cout << "connection ok!" << endl;
    else {
         cout << "connection fail: " << mysql_error(sock) << endl;
    }
    
    //connection character set
    cout << "connection character set: " << mysql_character_set_name(sock) << endl;

    //wait for posibility to check system/mysql sockets
    system("PAUSE");
    
    //closing connection
    mysql_close(sock);

    return EXIT_SUCCESS;
}

