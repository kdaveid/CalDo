﻿using CalDo.Functions;

namespace CalDo.Models
{
    public class ToDoVM
    {
        public string? Uid { get; set; }

        public string? Name { get; set; }

        public string? Description { get; set; }

        public DateTime? StartDT { get; set; }

        public DateTime? EndDT { get; set; }

        public DateTime? RepetitionUntil { get; set; }

        public bool RepetitionUntilForEver { get; set; }

        public string? Frequency { get; set; }

        public bool Enabled { get; set; }

        public int Interval { get; set; }

        public AlarmVM? Alarm { get; set; }

        public static ToDoVM From(CalendarEvent evt, bool enabled)
        {
            var rule = evt.RecurrenceRules.FirstOrDefault();

            var freq = FrequencyType.None;
            var ival = 0;
            DateTime? until = null;

            if (rule != null)
            {
                freq = rule.Frequency;
                ival = rule.Interval;
                until = rule.Until;
            }

            AlarmVM? a = AlarmVM.DisabledAlarm;
            if (evt.Alarms.Any())
            {
                a = AlarmVM.FromCalendarObj(evt.Alarms.First());
            }
            return new ToDoVM
            {
                Uid = evt.Uid,
                Name = evt.Summary,
                Description = evt.Description,
                StartDT = evt.DtStart.HasDate ? evt.DtStart.Value : null,
                EndDT = evt.DtEnd.HasDate ? evt.DtEnd.Value : null,
                Frequency = freq.ToString(),
                Interval = ival,
                Alarm = a, 
                Enabled = enabled,
                RepetitionUntil = until > DateTime.MinValue ? until : null,
                RepetitionUntilForEver = until <= DateTime.MinValue,
            };
        }

        internal static CalendarEvent ToCalendarEvent(ToDoVM item)
        {
            var @event = new CalendarEvent
            {
                Uid = item.Uid,
                Class = "PUBLIC",
                Summary = item.Name,
                Description = item.Description,
                DtStart = item.StartDT.HasValue ? new CalDateTime(item.StartDT.Value) : null,
                DtEnd = item.EndDT.HasValue ? new CalDateTime(item.EndDT.Value) : null,
            };

            if (item.Interval > 0) // more than once
            {
                var rrule = new RecurrencePattern(FrequencyConversions.FromString(item.Frequency ?? "None"), item.Interval);

                if (item.RepetitionUntil != null)
                {
                    rrule.Until = item.RepetitionUntil.Value;
                }

                @event.RecurrenceRules = new List<RecurrencePattern> { rrule };
            }

            if (!(string.IsNullOrEmpty(item.Alarm?.Trigger)))
            {
                @event.Alarms.Add(new Alarm
                {
                    Action = AlarmAction.Display,
                    Summary = item.Alarm.Summary,
                    Description = item.Alarm.Description,
                    Trigger = new Trigger(item.Alarm.Trigger)
                });
            }

            //var cal = new Calendar
            //{
            //    Method = "PUBLISH",
            //    Version = "2.0"
            //};

            //cal.Events.Add(@event);

            //var serializer = new CalendarSerializer();
            //var serializedCalendar = serializer.SerializeToString(cal);


            return @event;
        }

    }
}

