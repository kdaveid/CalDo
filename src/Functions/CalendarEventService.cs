using Ical.Net;
using Ical.Net.CalendarComponents;
using Ical.Net.Serialization;

namespace CalDo.Functions
{
    public class CalendarEventService
    {
        private static readonly string RootPath = Path.Combine(Directory.GetCurrentDirectory(), "Data");
        private static readonly string ToDoPath = Path.Combine(RootPath, "ToDoItems");
        private static readonly string ToDoDisabledPath = Path.Combine(ToDoPath, "Disabled");

        public IEnumerable<CalendarEvent> GetAll()
        {
            var files = Directory.GetFiles(ToDoPath, "*.ics").ToList();
            files.AddRange(Directory.GetFiles(ToDoDisabledPath, "*.ics"));

            return files
                .Select(s => ReadFile(s))
                .Where(s => s != null)
                .Cast<CalendarEvent>()
                .ToList();
        }

        public IEnumerable<CalendarEvent> GetEnabled()
        {
            var files = Directory.GetFiles(ToDoPath, "*.ics").ToList();

            return files
                .Select(s => ReadFile(s))
                .Where(s => s != null)
                .Cast<CalendarEvent>()
                .ToList();
        }

        public CalendarEvent? GetEnabled(string id)
            => ReadFile(Path.Combine(ToDoPath, $"{id}.ics"));

        public CalendarEvent? GetDisabled(string id)
            => ReadFile(Path.Combine(ToDoDisabledPath, $"{id}.ics"));

        private static CalendarEvent? ReadFile(string filePath)
        {
            if (!File.Exists(filePath))
            {
                return null;
            }

            using var fs = new FileStream(filePath, FileMode.Open,FileAccess.Read);
            var cal = Calendar.Load(fs);

            return cal.Events.First();
        }

        public void Save(CalendarEvent item, bool enabled)
        {
            var enabledFilePath = Path.Combine(ToDoPath, $"{item.Uid}.ics");
            var disabledFilePath = Path.Combine(ToDoDisabledPath, $"{item.Uid}.ics");

            if (enabled)
            {
                WriteFile(item, enabledFilePath);
                DeleteFileIfExists(disabledFilePath);
            }
            else
            {
                WriteFile(item, disabledFilePath);
                DeleteFileIfExists(enabledFilePath);
            }
        }

        private static void DeleteFileIfExists(string filePath)
        {
            if (File.Exists(filePath))
            {
                File.Delete(filePath);
            }
        }

        private static void WriteFile(CalendarEvent item, string path)
        {
            var serializedCal = SerializeEvent(item);

            File.WriteAllText(path, serializedCal);
        }

        private static string SerializeEvent(CalendarEvent e) => new CalendarSerializer().SerializeToString(new Calendar { Events = { e } });

    }
}
