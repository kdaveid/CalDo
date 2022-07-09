using CalDo.Functions;

namespace CalDo.Models
{
    public class ToDoVM
    {
        public string? Uid { get; set; }

        public string? Name { get; set; }

        public string? Description { get; set; }

        public DateTime? StartDT { get; set; }

        public DateTime? EndDT { get; set; }

        public string? Frequency { get; set; }

        public bool Enabled { get; set; }

        public int Interval { get; set; }

        public string? Alarm { get; set; }

        public static ToDoVM From(CalendarEvent evt)
        {
            var rule = evt.RecurrenceRules.FirstOrDefault();

            var freq = FrequencyType.None;
            var ival = 0;

            if (rule != null)
            {
                freq = rule.Frequency;
                ival = rule.Interval;
            }


            var defaultAlarm = new Alarm
            {
                Action = AlarmAction.Display,
                Description = "Reminder",
                Trigger = new Trigger(TimeSpan.Zero)
            };

            var serializedAlarm = new TriggerSerializer().SerializeToString(defaultAlarm);

            if (evt.Alarms.Any())
            {
                serializedAlarm = new TriggerSerializer().SerializeToString(evt.Alarms.First());
            }


            return new ToDoVM
            {
                Uid = evt.Uid,
                Name = evt.Summary,
                Description = evt.Description,
                StartDT = evt.DtStart.HasDate ? evt.DtStart.Date : null,
                EndDT = evt.DtEnd.HasDate ? evt.DtEnd.Date : null,
                Frequency = freq.ToString(),
                Interval = ival,
                Alarm = serializedAlarm// evt.Alarms.Any() ? evt.Alarms.First().
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
                Url = new Uri($"http://caldo.schatzinos.net/{item.Uid}"),
                RecurrenceRules = new List<RecurrencePattern> { new RecurrencePattern(FrequencyConversions.FromString(item.Frequency ?? "None"), item.Interval) },
            };
            if (item.Alarm is not null)
            {
                @event.Alarms.Add((Alarm)new TriggerSerializer().Deserialize(new StringReader(item.Alarm)));
            }

            return @event;
        }

    }
}

