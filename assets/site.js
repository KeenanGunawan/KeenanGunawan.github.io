const menuButton = document.querySelector('.mobile-menu');
const sidebar = document.querySelector('.sidebar');

if (menuButton && sidebar) {
  menuButton.addEventListener('click', () => {
    const isOpen = sidebar.classList.toggle('open');
    menuButton.setAttribute('aria-expanded', String(isOpen));
    menuButton.textContent = isOpen ? 'Close' : 'Menu';
  });
}

const searchInput = document.querySelector('[data-site-search]');
const searchItems = [...document.querySelectorAll('[data-search-item]')];
const portfolioCards = [...document.querySelectorAll('[data-project-category]')];
const filterButtons = [...document.querySelectorAll('[data-project-filter]')];
const filterStatus = document.querySelector('[data-project-filter-status]');
let activeProjectFilter = 'all';
let activeProjectSearch = '';

function applyProjectFilters() {
  if (!portfolioCards.length) return;
  let visibleCount = 0;
  portfolioCards.forEach((card) => {
    const categories = card.dataset.projectCategory.split(' ');
    const matchesCategory = activeProjectFilter === 'all' || categories.includes(activeProjectFilter);
    const matchesSearch = !activeProjectSearch || card.textContent.toLowerCase().includes(activeProjectSearch);
    card.hidden = !(matchesCategory && matchesSearch);
    if (!card.hidden) visibleCount += 1;
  });

  if (filterStatus) {
    const label = activeProjectFilter === 'all'
      ? ''
      : `${filterButtons.find((button) => button.dataset.projectFilter === activeProjectFilter)?.textContent.toLowerCase()} `;
    const searchNote = activeProjectSearch ? ` matching “${activeProjectSearch}”` : '';
    filterStatus.textContent = `Showing ${visibleCount} ${label}project${visibleCount === 1 ? '' : 's'}${searchNote}`;
  }
}

filterButtons.forEach((button) => {
  button.addEventListener('click', () => {
    activeProjectFilter = button.dataset.projectFilter;
    filterButtons.forEach((candidate) => {
      candidate.setAttribute('aria-pressed', String(candidate === button));
    });
    applyProjectFilters();
  });
});

if (searchInput) {
  searchInput.addEventListener('input', (event) => {
    const term = event.target.value.trim().toLowerCase();
    activeProjectSearch = term;
    searchItems.forEach((item) => {
      const content = `${item.textContent} ${item.dataset.searchItem}`.toLowerCase();
      item.hidden = Boolean(term) && !content.includes(term);
    });
    applyProjectFilters();
  });

  searchInput.addEventListener('keydown', (event) => {
    if (event.key !== 'Enter') return;
    if (portfolioCards.length) return;
    const firstMatch = searchItems.find((item) => !item.hidden && item.tagName === 'A');
    if (firstMatch) window.location.href = firstMatch.href;
  });
}

const tocLinks = [...document.querySelectorAll('.page-toc a')];
const tocSections = tocLinks
  .map((link) => document.querySelector(link.getAttribute('href')))
  .filter(Boolean);

if (tocSections.length && 'IntersectionObserver' in window) {
  const observer = new IntersectionObserver((entries) => {
    const visible = entries
      .filter((entry) => entry.isIntersecting)
      .sort((a, b) => a.boundingClientRect.top - b.boundingClientRect.top)[0];
    if (!visible) return;
    tocLinks.forEach((link) => {
      link.classList.toggle('current', link.getAttribute('href') === `#${visible.target.id}`);
    });
  }, { rootMargin: '-15% 0px -70% 0px', threshold: 0 });

  tocSections.forEach((section) => observer.observe(section));
}
