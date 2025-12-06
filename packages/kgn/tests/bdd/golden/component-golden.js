import React from 'react';

export const UserProfile = ({ name, email }) => {
  return (
    <div className="widget-userprofile">
      <h1>User Profile Widget</h1>
      <p>name: {name}</p>
      <p>email: {email}</p>
    </div>
  );
};