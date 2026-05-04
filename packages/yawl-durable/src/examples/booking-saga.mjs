/**
 * @file Booking Saga Example - Distributed transaction with compensation
 * @module @unrdf/yawl-durable/examples/booking-saga
 *
 * Demonstrates a real-world saga pattern: booking flight, hotel, and car rental.
 * If any step fails, all previous bookings are automatically cancelled.
 *
 * This shows how YAWL's event sourcing + cancellation regions implement
 * Temporal.io's saga pattern for distributed transactions.
 */

import { DurableWorkflowEngine } from '../engine.mjs';
import { executeSaga, createSagaWorkflow } from '../saga.mjs';

// =============================================================================
// Mock External Services
// =============================================================================

/**
 * Mock flight booking service
 */
const FlightService = {
  bookings: new Map(),

  async book(flightId, userId) {
    const bookingId = `FL-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;

    // Simulate network delay
    await new Promise(resolve => setTimeout(resolve, 100));

    // Simulate occasional failures (10% chance)
    if (Math.random() < 0.1) {
      throw new Error('Flight booking service unavailable');
    }

    this.bookings.set(bookingId, {
      bookingId,
      flightId,
      userId,
      status: 'CONFIRMED',
      bookedAt: new Date().toISOString(),
    });

    console.log(`✈️  Flight booked: ${bookingId} (flight: ${flightId})`);
    return this.bookings.get(bookingId);
  },

  async cancel(bookingId) {
    const booking = this.bookings.get(bookingId);
    if (!booking) {
      throw new Error(`Flight booking ${bookingId} not found`);
    }

    booking.status = 'CANCELLED';
    console.log(`✈️  Flight cancelled: ${bookingId}`);
  },
};

/**
 * Mock hotel booking service
 */
const HotelService = {
  bookings: new Map(),

  async book(hotelId, userId, checkIn, checkOut) {
    const bookingId = `HTL-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;

    await new Promise(resolve => setTimeout(resolve, 150));

    // Simulate occasional failures
    if (Math.random() < 0.1) {
      throw new Error('Hotel booking service unavailable');
    }

    this.bookings.set(bookingId, {
      bookingId,
      hotelId,
      userId,
      checkIn,
      checkOut,
      status: 'CONFIRMED',
      bookedAt: new Date().toISOString(),
    });

    console.log(`🏨 Hotel booked: ${bookingId} (hotel: ${hotelId})`);
    return this.bookings.get(bookingId);
  },

  async cancel(bookingId) {
    const booking = this.bookings.get(bookingId);
    if (!booking) {
      throw new Error(`Hotel booking ${bookingId} not found`);
    }

    booking.status = 'CANCELLED';
    console.log(`🏨 Hotel cancelled: ${bookingId}`);
  },
};

/**
 * Mock car rental service
 */
const CarRentalService = {
  bookings: new Map(),

  async book(carId, userId, pickupDate, returnDate) {
    const bookingId = `CAR-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;

    await new Promise(resolve => setTimeout(resolve, 120));

    // Simulate occasional failures
    if (Math.random() < 0.1) {
      throw new Error('Car rental service unavailable');
    }

    this.bookings.set(bookingId, {
      bookingId,
      carId,
      userId,
      pickupDate,
      returnDate,
      status: 'CONFIRMED',
      bookedAt: new Date().toISOString(),
    });

    console.log(`🚗 Car booked: ${bookingId} (car: ${carId})`);
    return this.bookings.get(bookingId);
  },

  async cancel(bookingId) {
    const booking = this.bookings.get(bookingId);
    if (!booking) {
      throw new Error(`Car rental booking ${bookingId} not found`);
    }

    booking.status = 'CANCELLED';
    console.log(`🚗 Car rental cancelled: ${bookingId}`);
  },
};

// =============================================================================
// Saga Definition
// =============================================================================

/**
 * Create travel booking saga workflow
 *
 * This saga books flight, hotel, and car in sequence.
 * If any booking fails, all previous bookings are compensated (cancelled).
 */
export function createBookingSaga() {
  return createSagaWorkflow({
    id: 'travel-booking-saga',
    name: 'Travel Booking Saga',
    version: '1.0.0',
    steps: [
      // Step 1: Book Flight
      {
        id: 'bookFlight',
        name: 'Book Flight',
        timeout: 30000,
        retryPolicy: {
          maxAttempts: 3,
          initialInterval: 1000,
          backoffCoefficient: 2,
        },
        handler: async (input) => {
          const booking = await FlightService.book(input.flightId, input.userId);
          return {
            ...input,
            flightBooking: booking,
          };
        },
        compensate: async (output) => {
          if (output.flightBooking) {
            await FlightService.cancel(output.flightBooking.bookingId);
          }
        },
      },

      // Step 2: Book Hotel
      {
        id: 'bookHotel',
        name: 'Book Hotel',
        timeout: 30000,
        retryPolicy: {
          maxAttempts: 3,
          initialInterval: 1000,
          backoffCoefficient: 2,
        },
        handler: async (input) => {
          const booking = await HotelService.book(
            input.hotelId,
            input.userId,
            input.checkIn,
            input.checkOut
          );
          return {
            ...input,
            hotelBooking: booking,
          };
        },
        compensate: async (output) => {
          if (output.hotelBooking) {
            await HotelService.cancel(output.hotelBooking.bookingId);
          }
        },
      },

      // Step 3: Book Car
      {
        id: 'bookCar',
        name: 'Book Car Rental',
        timeout: 30000,
        retryPolicy: {
          maxAttempts: 3,
          initialInterval: 1000,
          backoffCoefficient: 2,
        },
        handler: async (input) => {
          const booking = await CarRentalService.book(
            input.carId,
            input.userId,
            input.pickupDate,
            input.returnDate
          );
          return {
            ...input,
            carBooking: booking,
          };
        },
        compensate: async (output) => {
          if (output.carBooking) {
            await CarRentalService.cancel(output.carBooking.bookingId);
          }
        },
      },
    ],
  });
}

// =============================================================================
// Example Execution
// =============================================================================

/**
 * Run booking saga example
 */
export async function runBookingSagaExample() {
  console.log('='.repeat(80));
  console.log('TRAVEL BOOKING SAGA - Distributed Transaction Example');
  console.log('='.repeat(80));
  console.log('');

  const engine = new DurableWorkflowEngine();

  // Define saga workflow
  const sagaConfig = createBookingSaga();
  await engine.defineWorkflow(sagaConfig);

  // Example 1: Successful booking
  console.log('📝 Example 1: Successful booking');
  console.log('-'.repeat(80));

  const successInput = {
    userId: 'user-123',
    flightId: 'UA-1234',
    hotelId: 'marriott-downtown',
    carId: 'tesla-model3',
    checkIn: '2025-01-15',
    checkOut: '2025-01-20',
    pickupDate: '2025-01-15',
    returnDate: '2025-01-20',
  };

  try {
    const result = await executeSaga(engine, 'travel-booking-saga', successInput);

    if (result.success) {
      console.log('');
      console.log('✅ SAGA COMPLETED SUCCESSFULLY');
      console.log(`   Execution ID: ${result.executionId}`);
      console.log(`   Completed activities: ${result.completedActivities.join(', ')}`);
      console.log(`   Flight booking: ${result.output.flightBooking.bookingId}`);
      console.log(`   Hotel booking: ${result.output.hotelBooking.bookingId}`);
      console.log(`   Car booking: ${result.output.carBooking.bookingId}`);
    }
  } catch (error) {
    console.error('Saga failed:', error);
  }

  console.log('');
  console.log('='.repeat(80));

  // Example 2: Demonstrate compensation (force failure on car booking)
  console.log('📝 Example 2: Booking with compensation (car booking fails)');
  console.log('-'.repeat(80));

  // Temporarily make car service always fail
  const originalBook = CarRentalService.book;
  CarRentalService.book = async () => {
    throw new Error('Car rental service is down for maintenance');
  };

  try {
    const result = await executeSaga(engine, 'travel-booking-saga', successInput);

    if (!result.success) {
      console.log('');
      console.log('❌ SAGA FAILED - COMPENSATION EXECUTED');
      console.log(`   Execution ID: ${result.executionId}`);
      console.log(`   Error: ${result.error}`);
      console.log(`   Compensated activities: ${result.compensated.join(', ')}`);
      console.log('');
      console.log('   ✅ Flight booking was rolled back');
      console.log('   ✅ Hotel booking was rolled back');
      console.log('   ❌ Car booking never completed');
    }
  } catch (error) {
    console.error('Saga failed:', error);
  } finally {
    // Restore original function
    CarRentalService.book = originalBook;
  }

  console.log('');
  console.log('='.repeat(80));
  console.log('Example complete!');
  console.log('');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runBookingSagaExample().catch(console.error);
}
