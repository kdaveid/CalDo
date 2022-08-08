using System.Text;
using CalDo.Functions;
using Ical.Net;
using Ical.Net.Serialization;
using Microsoft.AspNetCore.Mvc;

namespace CalDo.Controllers
{
    [Route("api/calendar")]
    [ApiController]
    public class CalendarController : ControllerBase
    {
        private readonly CalendarEventService _service;
        private readonly ILogger<CalendarController> _logger;

        public CalendarController(CalendarEventService service, ILogger<CalendarController> logger)
        {
            _service = service;
            _logger = logger;
        }

        [HttpGet]
        public string Get()
        {
            _logger.LogInformation("getting calendar");
            return GetCalendarString();
        }

        [HttpGet("caldo.ics")]
        public IActionResult GetCalendar()
        {
            _logger.LogInformation("getting calendar");

            var serializedCalendar = GetCalendarString();

            var bytes = Encoding.UTF8.GetBytes(serializedCalendar);
            return File(bytes, "text/calendar", $"caldo.ics");
        }

        private string GetCalendarString()
        {
            var events = _service.GetEnabled();

            const string productId = "-//dkbe.ch//CalDo//NONSGML ical.net 4.0//EN";
            const string defaultProductId = "-//github.com/rianjs/ical.net//NONSGML ical.net 4.0//EN";

            var calendar = new Calendar();
            calendar.Events.AddRange(events);

            var serializer = new CalendarSerializer();
            return serializer.SerializeToString(calendar).Replace(defaultProductId, productId);
        }

        [HttpGet("url")]
        public string GetCalendarUrl()
        {
            var req = HttpContext.Request;
            return $"{req.Scheme}://{req.Host}/api/calendar/caldo.ics";
        }
    }
}
