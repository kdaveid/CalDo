namespace CalDo.Functions
{
    public static class CalendarUrlExtension
    {
        private static readonly string BaseUrl = Environment.GetEnvironmentVariable("BASE_URL") ?? "http://localhost:1234";

        public static IEnumerable<CalendarEvent?> ModifyCalendarEventUrl(this IEnumerable<CalendarEvent?> list)
             => list.Select(ModifyCalendarEventUrl);

        public static CalendarEvent? ModifyCalendarEventUrl(this CalendarEvent? evt)
        {
            if (evt == null) return evt;
            evt.Url = new Uri($"{BaseUrl}/events/{evt.Uid}");
            return evt;
        }
    }
}
