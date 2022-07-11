using CalDo.Models;
using Ical.Net.Serialization;
using static CalDo.Constants;

namespace CalDo.Functions
{
    public class CalendarEventService
    {
        public IEnumerable<ToDoVM> GetAll()
        {
            var files = GetEnabled()
                .Cast<CalendarEvent>()
                .Select(s => ToDoVM.From(s, true))
                .ToList();

            files.AddRange(GetDisabled()
                .Cast<CalendarEvent>()
                .Select(s => ToDoVM.From(s, false))
                .ToList());

            return files;
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

        public IEnumerable<CalendarEvent> GetDisabled()
        {
            var files = Directory.GetFiles(ToDoDisabledPath, "*.ics").ToList();

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

            using var fs = new FileStream(filePath, FileMode.Open, FileAccess.Read);
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

        public void Delete(string uid)
        {
            var enabledFilePath = Path.Combine(ToDoPath, $"{uid}.ics");
            var disabledFilePath = Path.Combine(ToDoDisabledPath, $"{uid}.ics");

            if (File.Exists(enabledFilePath))
            {
                DeleteFileIfExists(enabledFilePath);
            }
            else if (File.Exists(disabledFilePath))
            {
                DeleteFileIfExists(disabledFilePath);
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
