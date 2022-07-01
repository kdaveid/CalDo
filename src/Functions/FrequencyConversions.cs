using Ical.Net;

namespace CalDo.Functions
{
    public static class FrequencyConversions
    {
        public static FrequencyType FromString(string str)
        {
            if (str is null)
            {
                throw new ArgumentNullException(nameof(str));
            }

            if (!Enum.TryParse<FrequencyType>(str, out var freq))
            {
                throw new ArgumentException($"Is not a FrequencyType {str}");
            }

            return freq;
        }
    }
}
