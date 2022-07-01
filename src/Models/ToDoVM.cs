using CalDo.Functions;
using Ical.Net;
using Ical.Net.CalendarComponents;
using Ical.Net.DataTypes;

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

            return new ToDoVM
            {
                Uid = evt.Uid,
                Name = evt.Summary,
                Description = evt.Description,
                StartDT = evt.DtStart.HasDate ? evt.DtStart.Date : null,
                EndDT = evt.DtEnd.HasDate ? evt.DtEnd.Date : null,
                Frequency = freq.ToString(),
                Interval = ival,
            };
        }

        internal static CalendarEvent ToCalendarEvent(ToDoVM item)
        {
            return new CalendarEvent
            {
                Uid = item.Uid,
                Summary = item.Name,
                Description = item.Description,
                DtStart = item.StartDT.HasValue ? new CalDateTime(item.StartDT.Value) : null,
                DtEnd = item.EndDT.HasValue ? new CalDateTime(item.EndDT.Value) : null,
                Url = new Uri($"http://homecal.schatzinos.net/{item.Uid}"),
                RecurrenceRules = new List<RecurrencePattern> { new RecurrencePattern(FrequencyConversions.FromString(item.Frequency ?? "None"), item.Interval) }
            };
        }

    }
}

