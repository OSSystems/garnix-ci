// Leaves the app for another site, such as a forge's OAuth page.
export const goTo = (url: string): void => {
  window.location.assign(url);
};
