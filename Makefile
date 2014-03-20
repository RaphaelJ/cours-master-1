##############################################################################
# 
# INFO0045: Assignment #1
# 
# Makefile 
#
##############################################################################

target: info0045

JAVA            = java
JAVAC           = javac
RM              = rm
CP              = cp

JAVASRCS        = \
				info0045/Provider.java \
				info0045/AuthorityServer.java \
				info0045/AuthorityServerThread.java \
                info0045/Client.java \

JAVAOBJS        = $(JAVASRCS:.java=.class)

.SUFFIXES:      .class .java

.java.class: $*.java
	$(JAVAC) $(JAVACFLAGS) $*.java;

clean:
	$(RM) $(JAVAOBJS)

backup:
	$(CP) $(JAVASRCS) $(BACKUPDIR)

info0045:    $(JAVAOBJS)

