namespace CalDo.Models
{
    public class AlarmVM
    {
        public string? Summary { get; set; }
        public string? Description { get; set; }
        public string? Trigger { get; set; }

        internal static AlarmVM? FromCalendarObj(Alarm alarm)
        {
            if (alarm is null)
            {
                return null;
            }

            if (!alarm.Trigger.IsRelative)
            {
                throw new InvalidOperationException("only relative triggers for alarms are supported");
            }

            return new AlarmVM
            {
                 Description = alarm.Description,
                 Summary = "Reminder",
                 Trigger = new TimeSpanSerializer().SerializeToString(alarm.Trigger.Duration)
            };
        }
    }
}
