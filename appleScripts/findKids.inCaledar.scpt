set theStartDate to current date
--set theEndDate to

tell application "Calendar"
  set theCalendarName to "Co-parenting Schedule"
	set theCalendar to first calendar where its name = theCalendarName
	  tell calendar theCalendarName
      first event where its summary = "Kids: Srinath" and start date is greater than or equal to theStartDate
    -- count event where its summary = "Kids"
    end tell
end tell

