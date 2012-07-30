;; Copyright (C) 2012 bas smit (fbs)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(define-module (irc error-code)
  #:version (0 2 1)
  #:export (lookup-error-code
	    error-name
	    error-description))

(define error-table 
  '(
    (401 . (ERR_NOSUCHNICK . "<nickname> :No such nick/channel" ))
    (402 . (ERR_NOSUCHSERVER . "<server name> :No such server"))
    (403 . (ERR_NOSUCHCHANNEL . "<channel name> :No such channel"))
    (404 . (ERR_CANNOTSENDTOCHAN . "<channel name> :Cannot send to channel" ))
    (405 . (ERR_TOOMANYCHANNELS . "<channel name> :You have joined too many channels" ))
    (406 . (ERR_WASNOSUCHNICK . "<nickname> :There was no such nickname" ))
    (407 . (ERR_TOOMANYTARGETS . "<target> :<error code> recipients. <abort message>" ))
    (408 . (ERR_NOSUCHSERVICE . "<service name> :No such service" ))
    (409 . (ERR_NOORIGIN . ":No origin specified" ))
    (411 . (ERR_NORECIPIENT . ":No recipient given (<command>)" ))
    (412 . (ERR_NOTEXTTOSEND . ":No text to send"))
    (413 . (ERR_NOTOPLEVEL . "<mask> :No toplevel domain specified"))
    (414 . (ERR_WILDTOPLEVEL . "<mask> :Wildcard in toplevel domain" ))
    (415 . (ERR_BADMASK . "<mask> :Bad Server/host mask" ))
    (421 . (ERR_UNKNOWNCOMMAND . "<command> :Unknown command"))
    (422 . (ERR_NOMOTD . ":MOTD File is missing"))
    (423 . (ERR_NOADMININFO . "<server> :No administrative info available"))
    (424 . (ERR_FILEERROR . ":File error doing <file op> on <file>"))
    (431 . (ERR_NONICKNAMEGIVEN . ":No nickname given"))
    (432 . (ERR_ERRONEUSNICKNAME . "<nick> :Erroneous nickname"))
    (433 . (ERR_NICKNAMEINUSE . "<nick> :Nickname is already in use"))
    (436 . (ERR_NICKCOLLISION . "<nick> :Nickname collision KILL from <user>@<host>"))
    (437 . (ERR_UNAVAILRESOURCE . "<nick/channel> :Nick/channel is temporarily unavailable"))
    (441 . (ERR_USERNOTINCHANNEL . "<nick> <channel> :They arent on that channel"))
    (442 . (ERR_NOTONCHANNEL . "<channel> :Youre not on that channel"))
    (443 . (ERR_USERONCHANNEL . "<user> <channel> :is already on channel"))
    (444 . (ERR_NOLOGIN . "<user> :User not logged in"))
    (445 . (ERR_SUMMONDISABLED . ":SUMMON has been disabled"))
    (446 . (ERR_USERSDISABLED . ":USERS has been disabled"))
    (451 . (ERR_NOTREGISTERED . ":You have not registered"))
    (461 . (ERR_NEEDMOREPARAMS . "<command> :Not enough parameters"))
    (462 . (ERR_ALREADYREGISTRED . ":Unauthorized command (already registered)"))
    (463 . (ERR_NOPERMFORHOST . ":Your host isnt among the privileged"))
    (464 . (ERR_PASSWDMISMATCH . ":Password incorrect"))
    (465 . (ERR_YOUREBANNEDCREEP . ":You are banned from this server"))
    (466 . (ERR_YOUWILLBEBANNED  . "" ))
    (467 . (ERR_KEYSET . "<channel> :Channel key already set"))
    (471 . (ERR_CHANNELISFULL . "<channel> :Cannot join channel (+l)"))
    (472 . (ERR_UNKNOWNMODE . "<char> :is unknown mode char to me for <channel>"))
    (473 . (ERR_INVITEONLYCHAN . "<channel> :Cannot join channel (+i)"))
    (474 . (ERR_BANNEDFROMCHAN . "<channel> :Cannot join channel (+b)"))
    (475 . (ERR_BADCHANNELKEY . "<channel> :Cannot join channel (+k)"))
    (476 . (ERR_BADCHANMASK . "<channel> :Bad Channel Mask"))
    (477 . (ERR_NOCHANMODES . "<channel> :Channel doesnt support modes"))
    (478 . (ERR_BANLISTFULL . "<channel> <char> :Channel list is full"))
    (481 . (ERR_NOPRIVILEGES . ":Permission Denied- Youre not an IRC operator"))
    (482 . (ERR_CHANOPRIVSNEEDED . "<channel> :Youre not channel operator"))
    (483 . (ERR_CANTKILLSERVER .  ":You cant kill a server!"))
    (484 . (ERR_RESTRICTED . ":Your connection is restricted!"))
    (485 . (ERR_UNIQOPPRIVSNEEDED . ":Youre not the original channel operator"))
    (491 . (ERR_NOOPERHOST . ":No O-lines for your host"))
    (501 . (ERR_UMODEUNKNOWNFLAG . ":Unknown MODE flag"))
    (502 . (ERR_USERSDONTMATCH . ":Cannot change mode for other users"))))

(define (lookup-error-code code)
  (cond
   ((not (number? code)) #f)
   ((< 400 code 503) (cdr (assoc code error-table)))
   (else #f)))

(define (error-name dp)
  (if (pair? dp)
      (car dp)
      #f))

(define (error-description dp)
  (if (pair? dp)
      (cdr dp)
      #f))

