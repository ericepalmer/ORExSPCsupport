/// Eric E. Palme2
// 13 March 2017
// 	This displays that status of LITHOSP iterations

// Comple with
//cc -lncurses cur.c
/* More info:
			http://computer-programming-forum.com/47-c-language/213d520efb1e738c.htm
*/

#define WIDTH 60
#define STATLINE 21
#define FLAGLINE 2

#include <curses.h> 
#include <unistd.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <time.h> 
#include <unistd.h> 

int	lithosA [100];
int	lithosNum = 0;
int	runA  [100];
int	runNum = 0;
FILE	*elogF;

char	*logA [256];
int	logNum = 0;
int	flagEmail = 0;
int	flagRunning = 0;
char	userStr [100];
char	timeBuf [100];
char	deamonStr [100];
char	flagStr [100];
char *deamonID;			// the pid of the monitord deamon, if running
char	writeStr [255];

struct deamon_t {
	int running;
	int geometry;			// runs geometry when iteration complete
	int residuals;			// runs residuals when iteration complete
	int localloop;			// looks for and run localLoop.sh
	int find;			// runs find_nofit when iteration complete
	int email;			// sends email notification
	int terminate;		// ends after the current iteration
} dStat;

///////////////////////////////////////////////////////////
// logCommand /////////////////////////////////////////////
///////////////////////////////////////////////////////////
void logCommand (char *logStr) {
	char	buf [255];
	time_t	currT;
	char	*newStr;

	time (&currT);
	char *currTime = ctime(&currT);
	currTime [16] = '\0';
	sprintf (buf, "%s-%s", &(currTime[4]), logStr);

	newStr = (char *) malloc (sizeof (char)* strlen(buf));
	strcpy (newStr, buf);
	logA[logNum] = newStr;
	logNum++;

}//logcommand

///////////////////////////////////////////////////////////
// areYouSure /////////////////////////////////////////////
///////////////////////////////////////////////////////////
int areYouSure () {
	mvaddstr (5, 8, "ARE YOU SURE");
	mvaddstr (6, 8, "Y to continue");
	mvaddstr (7, 8, "> ");
	refresh();

	timeout (-1);				// blocking delay
	char ch = getch ();
	timeout (10000);			// back to 10 sec

	if (ch == 'Y') return 1;
	mvaddstr (11, 8, "Aborting (nevermind)");
	refresh();
	ch = getch ();	// dump the char, just want a timer/anykey

	return 0;

}//areyousure

///////////////////////////////////////////////////////////
// parseLine //////////////////////////////////////////////
//		Returns the nth item in the string, defined by white space
///////////////////////////////////////////////////////////
char *parseLine (char *workingStr, int	arg){
	int	i, start, end;
	int	cnt =0;
	int 	len = strlen (workingStr);
	int	word;
	char	*buf = (char *) malloc (100);

	if (workingStr [0] == ' ')			// start with a state
		word = 0;
	else
		word = 1;
	//fprintf (stderr, "%d--",  arg);

	end = len;
	start = 0;
	for (i=0; i<len; i++) {
		if (word) {					// currently looking at a word
			//fprintf (stderr, "*");
			if (workingStr [i] == ' ') {// found the end of a word
			//fprintf (stderr, "#");
				word = 0;
				cnt++;
				strncpy (buf, &(workingStr [start]), end - start+1);
				buf [end-start+1] = '\0';				// terminate the string
				//fprintf (stderr, "<%s>\n",  buf);
				if (cnt == arg)			// I found the argument, exiting
					return (buf);
			}//ifbuf
			end = i;					// keep resetting the end
		}//ifword
		else {						// not a word
			//fprintf (stderr, ".");
			if (workingStr [i] == ' ') 
				continue;				// still not a word
			if (workingStr [i] == '	') 
				continue;				// still not a word
			//fprintf (stderr, "^");
			start = i;					// found the start of a word
			word = 1;
			end = i+1;
		}//elseword
	}//for

	if (i == len) {			// if you got to the end of the line, take it
		end++;
		strncpy (buf, &(workingStr [start]), end - start);
		buf [end-start] = '\0';				// terminate the string
				//fprintf (stderr, "%s<<\n",  buf);
		cnt++;
	}//if
		
	if (arg != cnt) return (NULL);

	return (buf);

}//parseLine

///////////////////////////////////////////////////////////
// scanLine ///////////////////////////////////////////////
//		Reads in a single line at a time
///////////////////////////////////////////////////////////
int scanLine (FILE *in, char *buf){
	char ch;
	int	cnt = 0;
	
	if (feof(in)) return -1; 

	while ( (ch=fgetc(in)) != '\n') {
		if (feof(in)) return -1;
		buf [cnt++] = ch;
	}//while
	buf [cnt] = '\0';
	return cnt;

}//scanline


///////////////////////////////////////////////////////////
// This gets the number of lines and errors from the find_nofit routine
//		Returns the number of stars, -1 is a file error
///////////////////////////////////////////////////////////
int doFindNoFit () {
	int	val, numLines;
	char	str [255], buf [255];
	int	bang = 0;		// number of exclamation points
	int	star = 0;		// number of star errors
	int	plus = 0;		// number of plus errors
	int	dot = 0;		// number of . 
	int	space = 0;		// number of spaces 

	sprintf (buf, "ls *.INN  | wc -l > %s", writeStr);
	system (buf);

	FILE *fitF = fopen (writeStr, "r");
	if (fitF == NULL)  {
		fclose (fitF);
		return (-1);
	}//if

	val=scanLine(fitF, str);
	int numLandmarks = atof (str);
	fclose (fitF);

	sprintf (buf, "find_nofitP > %s", writeStr);
	system (buf);
	fitF = fopen (writeStr, "r");
	if (fitF == NULL)  {
		fclose (fitF);
		return (-1);
	}//if


	while ( (val=scanLine(fitF, str)) != -1) {		// bigloop
		if (str [14] == 'n') {
			mvaddstr (STATLINE, 4, str);
			fclose (fitF);
			return (-1);
		}//if

		numLines++;
		switch (str [0]) {
			case '!': bang++; break;
			case '*': star++; break;
			case '+': plus++; break;
			case '.': dot++; break;
			case ' ': space++; break;
		}// switch

	}//while
	fclose (fitF);

  	mvaddstr (6, 40, "Char	Number"); 
	sprintf (buf, "!	%d", bang);
  	mvaddstr (7, 40, buf); 
	sprintf (buf, "*	%d", star);
  	mvaddstr (8, 40, buf); 
	sprintf (buf, "+	%d", plus);
  	mvaddstr (9, 40, buf); 
	sprintf (buf, ".	%d", dot);
  	mvaddstr (10, 40, buf); 
	sprintf (buf, " 	%d", space-1);
  	mvaddstr (11, 40, buf); 

	int numDone = atof (str);
	sprintf (buf, "%d of %d", numDone, numLandmarks);
  	mvaddstr (12, 40, buf); 

	return star;
}//dofindnofit

///////////////////////////////////////////////////////////
// prepScreen /////////////////////////////////////////////
///////////////////////////////////////////////////////////
void prepScreen () {
	clear ();
   move (1, 1); 
	vline ('|', 21);
   move (1, WIDTH+1); 
	vline ('|', 21);

   move (1, 1); 
	hline ('-', WIDTH+1);
   move (22, 1); 
	hline ('-', WIDTH+1);
	//border ('|', '|', '-', '-', '+', '+', '+', '+' );
	
}//prepscreen

///////////////////////////////////////////////////////////
// Error //////////////////////////////////////////////////
///////////////////////////////////////////////////////////
int Error (char *str) {
	fprintf (stdout, "%s\n", str);
	fprintf (stderr, "%s\n", str);
	exit (-1);
}//error

///////////////////////////////////////////////////////////
// Notice //////////////////////////////////////////////////
///////////////////////////////////////////////////////////
void Notice (char *str) {
	mvaddstr (STATLINE, 4, str);
//	fprintf (stderr, "%s\n", str);
	refresh();
	//sleep (5);
}//Notice

///////////////////////////////////////////////////////////
// runComplete ////////////////////////////////////////////
//		Ends the run
///////////////////////////////////////////////////////////
void runComplete()  {
	char	buf [255];
	int star = doFindNoFit () ;
	sprintf (buf, "Complete: %d stars", star);
	logCommand (buf);

	sprintf (buf, "   Total Elpased: %s", timeBuf);
	logCommand (buf);

/*
	sprintf (buf, "   Average %3.1", timeBuf);
	logCommand (buf);
*/

	if (flagEmail) {
		sprintf (buf, "echo All done %s %s | mail -s spc-monitor %s@psi.edu", 
					userStr, buf, userStr);
		system (buf);
	}//if
	
}//runComplete

///////////////////////////////////////////////////////////
// readConfig /////////////////////////////////////////////
///////////////////////////////////////////////////////////
void readConfig()  {
	char	str [255];
	char	*valStr, *arg;
	int	val, err;
	strcpy (deamonStr, "             ");		// clear the status line
	strcpy (flagStr, "           ");		// clear the status line
	char	buf [256];

	dStat.running = 0;
	dStat.geometry = 0;
	dStat.residuals = 0;
	dStat.localloop = 0;
	dStat.find = 0;
	dStat.email = 0;
	dStat.terminate = 0;

	// Get metadata on the iteration
	sprintf (buf, "ps -o pid,command -u %s | grep -i monitord.sh | grep -v grep > %s", 
					userStr, writeStr);
	system (buf);

	FILE	*stat = fopen (writeStr, "r");

	if (stat == NULL)  {
		Notice ("Cannot open 377");
	}//
	else {
		val = scanLine(stat, str);
		if (val != -1)	 {
			dStat.running = 1;
			deamonID = parseLine (str, 1);		// get pid
		}//ifelse val
	}//elsestat
	fclose (stat);

	FILE *confF = fopen ("config/monitorConfig", "r");
	if (confF )  		// no configuration file, error
		while ((err = scanLine (confF, str)) != -1) { 				// can't read, we are done
			arg = parseLine (str, 1);		// get argument
			valStr = parseLine (str, 2);		// get value
			val = atoi (valStr);
			if (strcmp (arg, "geometry") == 0) dStat.geometry = val;
			if (strcmp (arg, "localloop") == 0) dStat.localloop = val;
			if (strcmp (arg, "find") == 0) dStat.find = val;
			if (strcmp (arg, "residuals") == 0) dStat.residuals = val;
			if (strcmp (arg, "email") == 0) dStat.email = val;
			if (strcmp (arg, "terminate") == 0) dStat.terminate = val;
		}//while
	fclose (confF);


	if (dStat.running) 
		strcpy (deamonStr, "Deamon - Running ");
	else
		strcpy (deamonStr, "Deamon - Not Running ");

/*
	int n = strlen (deamonStr);
	if (dStat.terminate) deamonStr [n++] = 'T';
	if (dStat.terminate) deamonStr [n++] = 'e';
	if (dStat.terminate) deamonStr [n++] = 'r';
	if (dStat.terminate) deamonStr [n++] = 'm';
	if (dStat.terminate) deamonStr [n++] = '\0';
*/

	if (dStat.geometry) flagStr [0] = 'G' ;
	if (dStat.geometry) flagStr [1] = '0' + dStat.geometry;
	if (dStat.find) flagStr [3] = 'F';
	if (dStat.residuals) flagStr [5] = 'R';
	if (dStat.localloop) flagStr [7] = 'L';
	if (dStat.email) flagStr [9] = 'E';
	if (dStat.terminate) flagStr [11] = 'T';
	
	
}//readConfig

///////////////////////////////////////////////////////////
// writeConfig ////////////////////////////////////////////
///////////////////////////////////////////////////////////
void writeConfig()  {

	FILE *confF = fopen ("config/monitorConfig", "w");
	if (confF == NULL) {
		mvaddstr (STATLINE, 5, "Can't open config/monitorConfig file - nothing saved");
		getch ();
		fclose (confF);
		return; 		// no configuration file, error
	}//if

	fprintf (confF, "geometry  %d \n", dStat.geometry);
	fprintf (confF, "localloop  %d \n", dStat.localloop);
	fprintf (confF, "find  %d \n", dStat.find);
	fprintf (confF, "residuals  %d \n", dStat.residuals);
	fprintf (confF, "email  %d \n", dStat.email);
	fprintf (confF, "terminate  %d \n", dStat.terminate);
	fclose (confF);

}//writeconfig

///////////////////////////////////////////////////////////
// deamon /////////////////////////////////////////////////
///////////////////////////////////////////////////////////
void deamon()  {
	int i, cnt;
	char 	*dispStr;
	char	buf[255];
	int	line = 5;

	prepScreen ();
	mvaddstr (3, 5, "Deamon Menu");
	//mvaddstr (4, 5, "ON");
	//mvaddstr (4, 10, "OFF");

	if (dStat.running) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	mvaddstr (line++, 15, "Deamon - Toggle (S)");

	if (dStat.geometry) {
		dispStr = "Geometry - Off (0) ";
		mvaddstr (line, 15, dispStr);
		if (dStat.geometry == 1)
			dispStr = "Geometry (2)";
		else
			dispStr = "Geometry (1)";
		mvaddstr (line, 35, dispStr);
		}//if
	else {
		mvaddstr (line, 15, "Geometry - Option 1 (1), Option 2 (2) ");
	}//else
	if (dStat.geometry == 1) 
		mvaddstr (line, 10, "On 1");
	else if (dStat.geometry == 2) 
		mvaddstr (line, 10, "On 2");
	else
		mvaddstr (line, 5, "Off");
	line++;

	if (dStat.find) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	mvaddstr (line++, 15, "find_nofit -  Toggle (F)");


	if (dStat.residuals) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	mvaddstr (line++, 15, "Residuals - Toggle (R)");

	if (dStat.localloop) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	mvaddstr (line++, 15, "localLoop.sh - Toggle (L)");

	if (dStat.email) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	dispStr = "E - Toggle email notificaitons";
	mvaddstr (line++, 15, "Email Notification - Toggle (E)");

	if (dStat.terminate) 
		mvaddstr (line, 10, "On");
	else
		mvaddstr (line, 5, "Off");
	mvaddstr (line++, 15, "Terminate (no more repeates) - Toggle (T)");

	mvaddstr (line++, 15, "N - Nevermind (leave this menu)");
	mvaddstr (line++, 5, "> ");

	timeout (-1);				// blocking delay
   char ch = getch(); 
	timeout (10000);			// 10 sec
	int	ret;
	int 	*listA ;
	char	*commandStr;

	switch (ch) {
		case 'q': 
		case 'Q': 
		case '': 
		case 'n': 
		case 'N':
			return;
			break;
		case 'S':
		case 's':
			if (dStat.running) {
				sprintf (buf, "kill %s", deamonID);
				system (buf);
			} else {
				system ("nohup /opt/local/spc/bin/monitord.sh &");
			}//ifrunning
			//deamon();
			break;
		case '0':
			dStat.geometry = 0;
			writeConfig ();
			deamon();
			break;
		case '1':
			dStat.geometry = 1;
			writeConfig ();
			deamon();
			break;
		case '2':
			dStat.geometry = 2;
			writeConfig ();
			deamon();
			break;
		case 'F':
		case 'f':
			dStat.find = !dStat.find;
			writeConfig ();
			deamon();
			break;
		case 'R':
		case 'r':
			dStat.residuals = !dStat.residuals;
			writeConfig ();
			deamon();
			break;
		case 'L':
		case 'l':
			dStat.localloop = !dStat.localloop;
			writeConfig ();
			deamon();
			break;
		case 'T':
		case 't':
			dStat.terminate = !dStat.terminate;
			writeConfig ();
			deamon();
			break;
		case 'E':
		case 'e':
			dStat.email = !dStat.email;
			writeConfig ();
			deamon();
			break;
	}//switch
	
}//deamon

///////////////////////////////////////////////////////////
// displayLog /////////////////////////////////////////////
//		Shows the last n items in the log
///////////////////////////////////////////////////////////
void displayLog(int start)  {
	int i, cnt;
	prepScreen ();
	
	mvaddstr (3, 8, "Log file");
	for (i=start, cnt =0; i<logNum; i++, cnt++) {
		mvaddstr (i+3, 4, logA [i]);
		if (cnt > 20) break;
	}//for 

	timeout (-1);				// blocking delay
   char ch = getch(); 
	timeout (10000);			// 10 sec
	switch (ch) {
		case 'n':
		case 'N':
			displayLog (cnt);
	};
}//displayLog

///////////////////////////////////////////////////////////
// commandline ////////////////////////////////////////////
//		Return 0 to redraw the screen.
//		Return 1 to hold the display
///////////////////////////////////////////////////////////
int commandLine()  {
	char *logStr = "null";
	char	goalCh;
	char	buf [255];
	int	num;

	prepScreen ();
	mvaddstr (3, 8, "Command Menu");
	mvaddstr (5, 8, "P - Pause LITHOSP");
	mvaddstr (6, 8, "C - Continue paused LITHOPS");
	mvaddstr (7, 8, "G - Gracefully shutdown (kill run.sh)");
	mvaddstr (8, 8, "K - Immediate kill of LITHOSP");
	mvaddstr (9, 8, "N - Nevermind (leave this menu)");
	mvaddstr (10, 8, "> ");

	timeout (-1);				// blocking delay
   char ch = getch(); 
	timeout (10000);			// 10 sec
	int	ret;
	int 	*listA ;
	char	*commandStr;

	switch (ch) {
		case 'q': 
		case 'Q': 
		case '': 
		case 'n': 
		case 'N':
			return (0);
			break;
		case 'P':
		case 'p':
			prepScreen ();
			logStr = "Pause. Halt the execution all LITHOSP";
			mvaddstr (3, 8, logStr);
			ret = areYouSure ();
			if (ret == 0) return 0;
			commandStr = "kill -STOP ";
			listA = lithosA;
			num = lithosNum;
			goalCh = 'P';
			break;
		case 'C':
		case 'c':
			prepScreen ();
			logStr = "Continue. Start the execution all LITHOSP";
			mvaddstr (3, 8, logStr);
			ret = areYouSure ();
			if (ret == 0) return 0;
			commandStr = "kill -CONT ";
			listA = lithosA;
			num = lithosNum;
			goalCh = 'C';
			break;
		case 'K':
		case 'k':
			prepScreen ();
			logStr = "Kill. Hard kill all LITHOSP";
			mvaddstr (3, 8, logStr);
			ret = areYouSure ();
			if (ret == 0) return 0;
			commandStr = "kill -9 ";
			listA = lithosA;
			num = lithosNum;
			goalCh = 'K';
			break;
		case 'G':
		case 'g':
			prepScreen ();
			logStr = "Graceful. Kill run.sh. LITHOSP will complete.";
			mvaddstr (3, 8, logStr);
			mvaddstr (4, 18, "No more will start");
			ret = areYouSure ();
			if (ret == 0) return 0;
			commandStr = "kill -9 ";
			listA = runA;
			num = runNum;
			goalCh = 'G';
			break;
	}//swithc
	refresh();

	// Display the commands
	char xStr [100];
	for (int i=0; i<=num; i++) {
		if (listA [i] == 0) break;
		sprintf (xStr, "%s %d", commandStr, listA [i]);
		mvaddstr (8+i, 10, xStr);
	}//
	refresh ();

	sprintf (buf, "Press %c to execute", goalCh);
	mvaddstr (STATLINE, 5, buf);

	timeout (-1);				// blocking delay
   ch = getch(); 
	timeout (10000);			// 10 sec

	// Execute
	if (ch == goalCh) {
		for (int i=0; i<=num; i++) {
			if (listA [i] == 0) break;
			sprintf (xStr, "%s %d", commandStr, listA [i]);
			system (xStr);
		}//for
		logCommand (logStr);
	}//if

	return (1);
	
}//commandline


///////////////////////////////////////////////////////////
// processRun /////////////////////////////////////////////
//		return -1 for failure
///////////////////////////////////////////////////////////
int processRun (char *timeStr) {
	FILE	*stat = fopen (writeStr, "r");
	char	str [256];
	char	*tmp;

	if (stat == NULL)  {
		Notice ("Cannot open 645tmp.txt");
		fclose (stat);
		return -1;
	}//
	
	int cnt = 0;
	int val;
	while ( (val=scanLine(stat, str)) != -1) {		// bigloo
/*
		if (val == -1) {
			Notice ("run.sh output does not exist");
			//return -1;
			break;
		}//if
*/

		tmp = parseLine (str, 1);		// get pid
		//fprintf (stderr, "%d %s\n", cnt, tmp);
		int pid = atoi (tmp);
		runA [cnt] = pid;
		tmp = parseLine (str, 2);		// get time
		strcpy (timeStr, tmp);
		fprintf (elogF, "%d %s\n", cnt, timeStr);
		cnt++;
	}//while
	runA [cnt] = 0;
	runNum = cnt+1;
	fclose (stat);

	return (0);

}//processrun

///////////////////////////////////////////////////////////
// main ///////////////////////////////////////////////////
///////////////////////////////////////////////////////////
int main(int argc, char *argv[]) 
{ 
	int mainPID = getpid();

	elogF = fopen ("log/elog.txt", "w");

	sprintf (writeStr, "/tmp/monitor-%d.txt", mainPID);

   initscr(); 
	timeout (10000);

	long	cnt = 0;
	char	buf [256], tmpStr [256];
	char	str [256];
	char	*timeStr, *holdStr, dispStr[255];
	char	lineStr [256];
	int	num;
	int	numProc, prevProc = -1;
	int	val;
	long	maxTime;
	long	newTime, sec;
	int	m, s, ms;

	// Init things
	prepScreen ();

	// Set the user name
	if (argc == 2)
		strcpy (userStr, argv [1]);
	else {
		// Get the user that is running
		sprintf (buf, "whoami > %s", writeStr);
		system (buf);
		FILE *stat = fopen (writeStr, "r");
		if (stat == NULL) 
			Error ("Cannot open whoami 710tmp.txt");
		val = scanLine(stat, str);
		if (val == -1) 
			Error ("Can't read whoami 713tmp.txt");
		strcpy (userStr, str);
		fclose (stat);
	}//else


	while (1) {
		cnt++;
		numProc = 0;
		maxTime = 0;

		// Display user id
		sprintf (tmpStr, "User: %s", userStr);
		mvaddstr (3,4, tmpStr);

		// Update Config File
		readConfig ();


		// Count
		sprintf (buf, "Click: %ld", cnt);
   	mvaddstr (5, WIDTH-10, buf); 

		// Get metadata on the iteration
		sprintf (buf, "ps -o pid,etime,command -u %s | grep -i run.sh | grep -v grep > %s", userStr, writeStr);
		system (buf);

		// Process the return suite of strings and load the list of pid for run.sh
		processRun (timeBuf);
		sprintf (buf, "Total Elapsed: %s", timeBuf);
		mvaddstr (4, 4, buf);

		// Display flags as needed
   	mvaddstr (FLAGLINE+1, 40, deamonStr); 

		if (dStat.terminate)  {
			attron(A_REVERSE);
   		mvaddstr (FLAGLINE+1, 57, "Term"); 
			attroff(A_REVERSE);
		}//if

   	mvaddstr (FLAGLINE+2, 47, flagStr); 

		// Get the number of processing and max elapsed time
		//sprintf (buf, "ps -o pid,time,command | grep -i LITHOSP | grep -v grep > %s", userStr, writeStr);
		sprintf (buf, "ps -u %s -o pid,time,command | grep -i LITHOSP | grep -v grep > %s", userStr, writeStr);
		system (buf);
		FILE *stat = fopen (writeStr, "r");
		if (stat == NULL)  {
			Notice ("ps for LITHOSP not generated");
			fclose (stat);
			continue;
		}//if

   	mvaddstr (7, 8, "PID"); 
   	mvaddstr (7, 18, "Duration"); 
		while ( (val=scanLine(stat, str)) != -1) {		// bigloop

			// processid
			holdStr = parseLine (str, 1);
   		mvaddstr (8+numProc, 8, holdStr); 
			lithosA [numProc] = atoi (holdStr);

			// time
			timeStr = parseLine (str, 2);
   		mvaddstr (8+numProc, 18, timeStr); 
			sscanf (timeStr, "%d:%d:%d", &m, &s, &ms);
			sec = m*60+s;

			// track max
			if (sec > maxTime) {
				maxTime = sec;
				strcpy (dispStr, timeStr);
			}//if

			numProc++;
		}//while
		fclose (stat);
		lithosNum = numProc;

		// Display the number of processes
		sprintf (buf, "Processes: %d", numProc);
   	mvaddstr (5, 4, buf); 

		// Display the longest running process
		sprintf (buf, "Max Time: %s", dispStr);
   	mvaddstr (6, 4, buf); 

		// Get the iteration number
		int iterationCount = 0;
		stat = fopen ("config/iterationCount", "r");
		if (stat == NULL) 
			Notice ("Cannot open iterationCount");
		else {
			val = scanLine(stat, str);
			if (val == -1) 
				Notice ("Can't read iterationCount");
			else
				iterationCount = atoi (str);
		}//else
		fclose (stat);

		// Work out the running status
		if (numProc) {
   		sprintf (buf, "LITHOSP Running %d", iterationCount); 
   		mvaddstr (FLAGLINE, 40, buf); 
		}//if
		else {
			if (flagRunning == 1) {							// The thing just stopped
				runComplete ();
			}//if
   		mvaddstr (FLAGLINE, 40, "LITHOSP Not Running"); 
		}//else

		flagRunning = numProc;								// Set the running status

		if (prevProc == -1)						// the first time
			prevProc = numProc;

		if (numProc != prevProc) {				// Some processes ended
			prevProc = numProc;
		}//if

		//Menu
   	sprintf (buf, "LITHOSP Running %d", iterationCount); 
   	mvaddstr (FLAGLINE, 4, "Menu [DLFCV] > "); 

   	refresh(); 

		// Test for key press and respond
		//move (STATLINE, WIDTH);
   	char ch = getch(); 
		switch (ch) {
			case 'q':			// Quit and reset the terminal window
			case 'Q': 
					mvaddstr (22, 1, "Exiting");
   				endwin(); 
					printf ("\n\n");
					exit (0);
					break;
			case 'm': 			// toggle the email flag
			case 'M': 
					flagEmail = !flagEmail;
					continue;
					break;
			case 'f': 
			case 'F': 
					doFindNoFit () ;
					continue;
					break;
			case 'c': 
			case 'C': 
					commandLine (); 
					break;
			case 'd': 
			case 'D': 
					deamon (); 
					break;
			case 'v': 
			case 'V': 
					mvaddstr (STATLINE, 4, "   Version 1.06                                     ");		// Version old status
					continue;
					break;
			case 'L': 
			case 'l': 
					displayLog (0);
					break;
					break;
			case 'H': 
			case 'h': 
			case '?': 
					mvaddstr (STATLINE, 4, "h help, q quit, f find_nf, c cmnd, l log, d deamon");		// Help
					continue;
					break;
		}//switch

		prepScreen ();

	}//while

   endwin(); 
}
