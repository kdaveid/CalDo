using CalDo.Models;
using static CalDo.Constants;

namespace CalDo.Functions
{
    public class ToDoEventService
    {
        private static string SerializeEvent(ToDoEvent e) => System.Text.Json.JsonSerializer.Serialize(e);
        private static string FilePath(ToDoEvent item) => Path.Combine(EventsPath, $"{item.CalendarToDoId}_{item.EventId}.json");

        public IEnumerable<ToDoEvent> GetAll(string calendarEventId)
        {
            var files = Directory.GetFiles(EventsPath, $"{calendarEventId}*.json").ToList();
            return files.Select(s => ReadFile(s)).ToList();
        }

        public ToDoEvent Get(string calendarEventId, int id)
            => ReadFile(Path.Combine(EventsPath, $"{calendarEventId}_{id}.json"));

        private static ToDoEvent ReadFile(string filePath)
        {
            if (!File.Exists(filePath))
            {
                return null;
            }
            using var fs = new FileStream(filePath, FileMode.Open);
            return System.Text.Json.JsonSerializer.Deserialize<ToDoEvent>(fs) ?? new ToDoEvent();
        }

        public void Save(ToDoEvent item)
        {
            if (item.EventId == 0)
            {
                item.EventId = GetAll(item.CalendarToDoId).Count() + 1;
            }

            File.WriteAllText(FilePath(item), SerializeEvent(item));
        }

        public void Delete(ToDoEvent item)
        {
            DeleteFileIfExists(FilePath(item));
        }

        private static void DeleteFileIfExists(string filePath)
        {
            if (File.Exists(filePath))
            {
                File.Delete(filePath);
            }
        }
    }
}
