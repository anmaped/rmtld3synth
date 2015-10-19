#ifndef _MONITOR_EVENT_H_
#define _MONITOR_EVENT_H_

#include <time.h>

#include "time_compat.h"


/**
 * Data container that holds event data and keeps a timestamp of the event.
 *
 * Timestamps are unsigned integers that are dependent of the architecture.
 *
 * @author Humberto Carvalho (1129498@isep.ipp.pt)
 * @date
 */
template<typename T>
class Event {
private:
    /** Data container for this Event. */
    T data;
    /** Creation time for this Event */
    timespan time;

public:
    /**
     * Instantiates a new event.
     *
     * Instantiates a new event, nothing is zeroed out.
     */
    Event();

    /**
     * Instantiates a new event with data as this event.
     *
     * data is instantiated by a copy constructor,
     * time is instantiated by POSIX clock_gettime using CLOCK_REALTIME,
     *
     * @param data a constant reference to the data.
     *
     * @see time.h
     */
    //Event(const T &data);

    /**
     * Instantiates a new event with data and time parameters.
     *
     * Both  data and  time are instantiated using copy constructors.
     *
     * @param data a constant reference to the data being copied.
     * @param time constant reference to the time being copied.
     */
    Event(const T &data, const timespan &time);


    /**
     * Gets the event current data.
     *
     * @return a reference to this events Data.
     */
    T &getData();

    /**
    * Gets the event current data.
    *
    * @return a reference to this events Data.
    */
    const T &getData() const;

    /**
     * Gets the event creation time.
     *
     * @return the event current time.
     */
    timespan getTime();


    void setTime(timespan t);

    /**
    * Gets the event creation time.
    *
    * @return the event current time.
    */
    const timespan &getTime() const;

    /**
     * Checks if this object is null.
     *
     * An object is determined to be null if the current time is zeroed out,
     * that is if time.tv_sec is 0 and time_.tv_nsec is 0.
     *
     * @return whether this is a nullObject.
     */
    bool isNull() const;

    /**
     * Copies the Events data pointed by event to this object.
     *
     * @param event The volatile data to copy to this object.
     */
    Event<T> &operator=(const Event<T> *event);

    /**
     * Checks if this event is < than Event  event.
     *
     * @param event Another event.
     * @return whether this event is < than Event  event.
     */
    bool operator<(const Event &event) const;

    /**
    * Checks if this event is <= than Event  event.
    *
    * @param event Another event.
    * @return whether this event is <= than Event  event.
    */
    bool operator<=(const Event &event) const;

    /**
    * Checks if this event is > than Event  event.
    *
    * @param event Another event.
    * @return whether this event is > than Event  event.
    */
    bool operator>(const Event &event) const;

    /**
    * Checks if this event is >= than Event  event.
    *
    * @param event Another event.
    * @return whether this event is >= than Event  event.
    */
    bool operator>=(const Event &event) const;

    /**
    * Checks if this event is == to Event  event.
    *
    * @param event Another event.
    * @return whether this event is == to Event  event.
    */
    bool operator==(const Event &event) const;

    /**
    * Checks if this event is != than Event  event.
    *
    * @param event Another event.
    * @return whether this event is != than Event  event.
    */
    bool operator!=(const Event &event) const;

    /**
    * Checks if this event is < than timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is < than timespec  time.
    */
    bool operator<(const timespan &time) const;

    /**
    * Checks if this event is <= than timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is <= than timespec  time.
    */
    bool operator<=(const timespan &time) const;

    /**
    * Checks if this event is > than timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is > than timespec  time.
    */
    bool operator>(const timespan &time) const;

    /**
    * Checks if this event is >= than timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is >= than timespec  time.
    */
    bool operator>=(const timespan &time) const;

    /**
    * Checks if this event is == to timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is == to timespec  time.
    */
    bool operator==(const timespan &time) const;

    /**
    * Checks if this event is != than timespec  time.
    *
    * @param time a timespec as defined by time.h
    * @return whether this event is != than timespec  time.
    */
    bool operator!=(const timespan &time) const;

    /**
    * Checks if timespec time is < than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is < than Event  event.
    */
    template<typename D>
    friend bool operator<(const timespan &time, const Event<D> &event);

    /**
    * Checks if timespec time is < than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is < than Event  event.
    */
    template<typename D>
    friend bool operator<=(const timespan &time, const Event<D> &event);

    /**
    * Checks if timespec  time is <= than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is <= than Event  event.
    */
    template<typename D>
    friend bool operator>(const timespan &time, const Event<D> &event);

    /**
    * Checks if timespec  time is > than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is > than Event  event.
    */
    template<typename D>
    friend bool operator>=(const timespan &time, const Event<D> &event);

    /**
    * Checks if timespec  time is == than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is == than Event  event.
    */
    template<typename D>
    friend bool operator==(const timespan &time, const Event<D> &event);

    /**
    * Checks if timespec  time is != than Event  event.
    *
    * @param time a timespec as defined by time.h
    * @param event an event
    *
    * @return whether timespec  time is != than Event  event.
    */
    template<typename D>
    friend bool operator!=(const timespan &time, const Event<D> &event);
};

template<typename T>
Event<T>::Event() :
    data(0),
    time(0)
{

}

template<typename T>
Event<T>::Event(const T &ddata, const timespan &ttime) :
    data(ddata),
    time(ttime)
{

}

template<typename T>
T &Event<T>::getData() {
    return data;
}

template<typename T>
const T &Event<T>::getData() const {
    return data;
}

template<typename T>
timespan Event<T>::getTime() {
    return time;
}

template<typename T>
void Event<T>::setTime(timespan t) {
    time = t;
}

template<typename T>
const timespan &Event<T>::getTime() const {
    return time;
}

template<typename T>
bool Event<T>::isNull() const
{
    // [TODO] this function is incorrect
    return time == 0;
}

template<typename T>
Event<T> &Event<T>::operator=(const Event *event) {
    data = event->data;
    time = event->time;
    return *this;
}

template<typename T>
bool Event<T>::operator<(Event const &event) const {
    return time < event.time;
}

template<typename T>
bool Event<T>::operator<=(Event const &event) const {
    return time <= event.time;
}

template<typename T>
bool Event<T>::operator>(Event const &event) const {
    return time > event.time;
}

template<typename T>
bool Event<T>::operator>=(Event const &event) const {
    return time >= event.time;
}

template<typename T>
bool Event<T>::operator==(Event<T> const &event) const {
    return time == event.time && data == event.data;
}

template<typename T>
bool Event<T>::operator!=(Event<T> const &event) const {
    return time != event.time;
}

template<typename T>
bool Event<T>::operator<(const timespan &ttime) const {
    return this->time < ttime;
}

template<typename T>
bool Event<T>::operator<=(const timespan &ttime) const {
    return this->time <= ttime;
}

template<typename T>
bool Event<T>::operator>(const timespan &ttime) const {
    return this->time > ttime;
}

template<typename T>
bool Event<T>::operator>=(const timespan &ttime) const {
    return this->time >= ttime;
}

template<typename T>
bool Event<T>::operator==(const timespan &ttime) const {
    return this->time == ttime;
}

template<typename T>
bool Event<T>::operator!=(const timespan &ttime) const {
    return this->time != ttime;
}

template<typename T>
bool operator<(const timespan &ttime, const Event<T> &event) {
    return ttime < event.time;
}

template<typename T>
bool operator<=(const timespan &ttime, const Event<T> &event) {
    return ttime <= event.time;
}

template<typename T>
bool operator>(const timespan &ttime, const Event<T> &event) {
    return ttime > event.time;
}

template<typename T>
bool operator>=(const timespan &ttime, const Event<T> &event) {
    return ttime >= event.time;
}

template<typename T>
bool operator==(const timespan &ttime, const Event<T> &event) {
    return ttime == event.time;
}

template<typename T>
bool operator!=(const timespan &ttime, const Event<T> &event) {
    return ttime != event.time;
}


#endif //_MONITOR_EVENT_H_
