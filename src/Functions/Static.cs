namespace CalDo
{
    public static class Constants
    {
        public static readonly string RootPath = Path.Combine(Directory.GetCurrentDirectory(), "Data");
        public static readonly string EventsPath = Path.Combine(RootPath, "Events");
        public static readonly string ToDoPath = Path.Combine(RootPath, "ToDoItems");
        public static readonly string ToDoDisabledPath = Path.Combine(ToDoPath, "Disabled");

        public static void Initialize()
        {
            Directory.CreateDirectory(RootPath);
            Directory.CreateDirectory(EventsPath);
            Directory.CreateDirectory(ToDoPath);
            Directory.CreateDirectory(ToDoDisabledPath);
        }
    }
}