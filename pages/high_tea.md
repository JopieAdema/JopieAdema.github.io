---
permalink: /high_tea/
title: High Tea
---
<style>
/* ---- High Tea page ---- */
.ht-card {
  cursor: pointer;
  border: 1px solid #dee2e6;
  border-radius: 6px;
  padding: 1rem 1.1rem;
  margin-bottom: 1.25rem;
  background: #fff;
  transition: box-shadow 0.15s ease, transform 0.15s ease;
}
.ht-card:hover {
  box-shadow: 0 4px 14px rgba(1, 143, 89, 0.20);
  transform: translateY(-2px);
}
.ht-card h5 {
  margin-top: 0;
  margin-bottom: 0.4rem;
  font-size: 1rem;
  color: #018F59;
}
.ht-card .ht-card-desc {
  font-size: 0.875rem;
  color: #555;
  margin-bottom: 0.5rem;
  line-height: 1.4;
  display: -webkit-box;
  -webkit-line-clamp: 3;
  -webkit-box-orient: vertical;
  overflow: hidden;
}
.ht-btn-primary {
  background-color: #018F59;
  border-color: #018F59;
  color: #fff;
}
.ht-btn-primary:hover,
.ht-btn-primary:focus {
  background-color: #016944;
  border-color: #016944;
  color: #fff;
}
.ht-note {
  border-left: 3px solid #018F59;
  padding: 0.5rem 0.75rem;
  margin-bottom: 0.9rem;
  background: #f8fffe;
  border-radius: 0 4px 4px 0;
}
.ht-note p {
  margin: 0 0 0.25rem;
  font-size: 0.9375rem;
  line-height: 1.4;
}
.ht-meta {
  font-size: 0.75rem;
  color: #888;
  margin: 0;
}
.ht-empty {
  text-align: center;
  padding: 2.5rem 0;
  color: #888;
}
.ht-empty p {
  margin: 0;
  font-style: italic;
}
.ht-back-link {
  color: #018F59;
  font-size: 0.9rem;
  text-decoration: none;
  padding-left: 0;
}
.ht-back-link:hover {
  color: #016944;
  text-decoration: underline;
}
.ht-greeting {
  font-size: 0.875rem;
  color: #555;
}
.ht-small-link {
  font-size: 0.8rem;
  color: #008AFF;
}
.ht-modal-hint {
  font-size: 0.875rem;
  color: #666;
  margin-bottom: 0.75rem;
}
</style>

<!-- Identity modal (first visit) -->
<div class="modal fade" id="identityModal" tabindex="-1" role="dialog"
     aria-labelledby="identityModalLabel" aria-modal="true"
     data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm modal-dialog-centered" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="identityModalLabel">Who are you?</h5>
      </div>
      <div class="modal-body">
        <p class="ht-modal-hint">Your name will appear next to notes you add.</p>
        <input id="ht-name-input" type="text" class="form-control"
               placeholder="Your name" maxlength="40" autocomplete="off">
      </div>
      <div class="modal-footer">
        <button id="ht-save-name" type="button" class="btn ht-btn-primary">Let's go</button>
      </div>
    </div>
  </div>
</div>

<!-- Add / edit idea modal -->
<div class="modal fade" id="ideaModal" tabindex="-1" role="dialog"
     aria-labelledby="ideaModalLabel">
  <div class="modal-dialog modal-dialog-centered" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="ideaModalLabel">New idea</h5>
        <button type="button" class="close" data-dismiss="modal" aria-label="Close">
          <span aria-hidden="true">&times;</span>
        </button>
      </div>
      <div class="modal-body">
        <input type="hidden" id="ht-idea-edit-id">
        <div class="form-group">
          <label for="ht-idea-title">Title</label>
          <input id="ht-idea-title" type="text" class="form-control"
                 placeholder="One-line title" maxlength="100">
        </div>
        <div class="form-group">
          <label for="ht-idea-desc">Description</label>
          <textarea id="ht-idea-desc" class="form-control" rows="3"
                    placeholder="What's the idea? A sentence or two."></textarea>
        </div>
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-outline-secondary" data-dismiss="modal">Cancel</button>
        <button id="ht-save-idea" type="button" class="btn ht-btn-primary">Save</button>
      </div>
    </div>
  </div>
</div>

<!-- Main view -->
<div id="ht-main-view">
  <div class="d-flex justify-content-between align-items-center mb-3">
    <p class="ht-greeting mb-0">
      Hello, <strong id="ht-author-display"></strong>.
      <a href="#" id="ht-change-name" class="ht-small-link">Change name</a>
    </p>
    <button id="ht-add-idea-btn" type="button" class="btn ht-btn-primary">+ New idea</button>
  </div>
  <div id="ht-empty-state" class="ht-empty d-none">
    <p>No ideas yet — add the first one!</p>
  </div>
  <div id="ht-card-grid" class="row"></div>
</div>

<!-- Detail view -->
<div id="ht-detail-view" class="d-none">
  <button id="ht-back-btn" type="button" class="btn btn-link ht-back-link px-0 mb-3">
    &larr; Back to all ideas
  </button>
  <div class="d-flex justify-content-between align-items-start mb-1">
    <h2 id="ht-detail-title" class="mb-0"></h2>
    <button id="ht-edit-idea-btn" type="button"
            class="btn btn-sm btn-outline-secondary ml-2 flex-shrink-0">Edit</button>
  </div>
  <p id="ht-detail-meta" class="ht-meta mt-1"></p>
  <p id="ht-detail-desc" class="mt-2"></p>
  <hr>
  <h4>Notes</h4>
  <div id="ht-notes-list"></div>
  <div id="ht-empty-notes" class="ht-empty d-none">
    <p>No notes yet — be the first to add one.</p>
  </div>
  <div class="mt-3">
    <textarea id="ht-note-input" class="form-control mb-2" rows="2"
              placeholder="Add a note or sub-idea… (Ctrl+Enter to submit)"></textarea>
    <button id="ht-add-note-btn" type="button" class="btn ht-btn-primary">Add note</button>
  </div>
</div>

<script>
(function () {
  'use strict';

  var KEY_IDEAS  = 'high_tea_ideas';
  var KEY_AUTHOR = 'high_tea_author';
  var currentId  = null;

  /* Storage */
  function loadIdeas() {
    try { return JSON.parse(localStorage.getItem(KEY_IDEAS)) || []; }
    catch (e) { return []; }
  }
  function saveIdeas(ideas) {
    localStorage.setItem(KEY_IDEAS, JSON.stringify(ideas));
  }
  function loadAuthor() { return localStorage.getItem(KEY_AUTHOR) || ''; }
  function saveAuthor(n) { localStorage.setItem(KEY_AUTHOR, n); }
  function uid(p) { return p + '-' + Date.now(); }

  /* Hash routing */
  function getHashId() {
    var h = window.location.hash;
    return (h && h.indexOf('#idea-') === 0) ? h.slice(6) : null;
  }
  function setHash(id) {
    history.pushState(null, '', id ? '#idea-' + id : window.location.pathname);
  }

  /* Utilities */
  function esc(s) {
    return String(s || '')
      .replace(/&/g,'&amp;').replace(/</g,'&lt;')
      .replace(/>/g,'&gt;').replace(/"/g,'&quot;');
  }
  function fmtDate(iso) {
    if (!iso) return '';
    var d = new Date(iso);
    var m = ['Jan','Feb','Mar','Apr','May','Jun',
             'Jul','Aug','Sep','Oct','Nov','Dec'][d.getMonth()];
    return d.getDate() + ' ' + m + ' ' + d.getFullYear();
  }

  /* Main view */
  function renderMain() {
    currentId = null;
    document.getElementById('ht-main-view').classList.remove('d-none');
    document.getElementById('ht-detail-view').classList.add('d-none');
    document.getElementById('ht-author-display').textContent = loadAuthor() || 'Anonymous';

    var ideas = loadIdeas().slice().sort(function (a, b) {
      return new Date(b.createdAt) - new Date(a.createdAt);
    });
    var grid  = document.getElementById('ht-card-grid');
    var empty = document.getElementById('ht-empty-state');
    grid.innerHTML = '';

    if (!ideas.length) { empty.classList.remove('d-none'); return; }
    empty.classList.add('d-none');

    ideas.forEach(function (idea) {
      var n   = idea.notes ? idea.notes.length : 0;
      var col = document.createElement('div');
      col.className = 'col-12 col-sm-6';
      col.innerHTML =
        '<div class="ht-card" data-id="' + esc(idea.id) + '">' +
          '<h5>' + esc(idea.title) + '</h5>' +
          '<p class="ht-card-desc">' + esc(idea.description) + '</p>' +
          '<p class="ht-meta">' + esc(idea.author) + ' &middot; ' +
            fmtDate(idea.createdAt) + ' &middot; ' +
            (n === 1 ? '1 note' : n + ' notes') + '</p>' +
        '</div>';
      col.querySelector('.ht-card').addEventListener('click', function () {
        setHash(idea.id);
        renderDetail(idea.id);
      });
      grid.appendChild(col);
    });
  }

  /* Detail view */
  function renderDetail(id) {
    var ideas = loadIdeas();
    var idea  = ideas.filter(function (i) { return i.id === id; })[0];
    if (!idea) { setHash(null); renderMain(); return; }

    currentId = id;
    document.getElementById('ht-main-view').classList.add('d-none');
    document.getElementById('ht-detail-view').classList.remove('d-none');
    document.getElementById('ht-detail-title').textContent = idea.title;
    document.getElementById('ht-detail-meta').textContent =
      'Added by ' + idea.author + ' on ' + fmtDate(idea.createdAt);
    document.getElementById('ht-detail-desc').textContent = idea.description;

    var notes = idea.notes || [];
    var list  = document.getElementById('ht-notes-list');
    var emptyN = document.getElementById('ht-empty-notes');
    list.innerHTML = '';

    if (!notes.length) {
      emptyN.classList.remove('d-none');
    } else {
      emptyN.classList.add('d-none');
      notes.forEach(function (note) {
        var el = document.createElement('div');
        el.className = 'ht-note';
        el.innerHTML =
          '<p>' + esc(note.text) + '</p>' +
          '<span class="ht-meta">' + esc(note.author) +
          ' &middot; ' + fmtDate(note.createdAt) + '</span>';
        list.appendChild(el);
      });
    }
  }

  /* Add / save idea */
  function openIdeaModal(id) {
    var ideas = id ? loadIdeas().filter(function (i) { return i.id === id; }) : [];
    var idea  = ideas[0] || null;
    document.getElementById('ht-idea-edit-id').value  = idea ? idea.id : '';
    document.getElementById('ht-idea-title').value    = idea ? idea.title : '';
    document.getElementById('ht-idea-desc').value     = idea ? idea.description : '';
    document.getElementById('ideaModalLabel').textContent = idea ? 'Edit idea' : 'New idea';
    $('#ideaModal').modal('show');
  }

  function saveIdea() {
    var title  = document.getElementById('ht-idea-title').value.trim();
    var desc   = document.getElementById('ht-idea-desc').value.trim();
    var editId = document.getElementById('ht-idea-edit-id').value;
    if (!title) { document.getElementById('ht-idea-title').focus(); return; }

    var ideas  = loadIdeas();
    var author = loadAuthor() || 'Anonymous';

    if (editId) {
      ideas = ideas.map(function (i) {
        if (i.id === editId) { i.title = title; i.description = desc; }
        return i;
      });
    } else {
      ideas.push({ id: uid('ht'), title: title, description: desc,
                   author: author, createdAt: new Date().toISOString(), notes: [] });
    }

    saveIdeas(ideas);
    $('#ideaModal').modal('hide');
    if (editId && currentId === editId) { renderDetail(editId); }
    else { setHash(null); renderMain(); }
  }

  /* Add note */
  function addNote() {
    var text = document.getElementById('ht-note-input').value.trim();
    if (!text || !currentId) return;
    var ideas  = loadIdeas();
    var author = loadAuthor() || 'Anonymous';
    ideas = ideas.map(function (i) {
      if (i.id === currentId) {
        i.notes = i.notes || [];
        i.notes.push({ id: uid('nt'), text: text,
                       author: author, createdAt: new Date().toISOString() });
      }
      return i;
    });
    saveIdeas(ideas);
    document.getElementById('ht-note-input').value = '';
    renderDetail(currentId);
  }

  /* Boot */
  function boot() {
    var h = getHashId();
    if (h) { renderDetail(h); } else { renderMain(); }
  }

  /* Event wiring */
  document.addEventListener('DOMContentLoaded', function () {

    document.getElementById('ht-save-name').addEventListener('click', function () {
      var n = document.getElementById('ht-name-input').value.trim();
      if (!n) { document.getElementById('ht-name-input').focus(); return; }
      saveAuthor(n);
      $('#identityModal').modal('hide');
      boot();
    });
    document.getElementById('ht-name-input').addEventListener('keydown', function (e) {
      if (e.key === 'Enter') document.getElementById('ht-save-name').click();
    });

    document.getElementById('ht-add-idea-btn').addEventListener('click', function () {
      openIdeaModal(null);
    });
    document.getElementById('ht-save-idea').addEventListener('click', saveIdea);
    document.getElementById('ht-idea-desc').addEventListener('keydown', function (e) {
      if (e.key === 'Enter' && e.ctrlKey) saveIdea();
    });

    document.getElementById('ht-edit-idea-btn').addEventListener('click', function () {
      if (currentId) openIdeaModal(currentId);
    });

    document.getElementById('ht-back-btn').addEventListener('click', function (e) {
      e.preventDefault();
      setHash(null);
      renderMain();
    });

    document.getElementById('ht-add-note-btn').addEventListener('click', addNote);
    document.getElementById('ht-note-input').addEventListener('keydown', function (e) {
      if (e.key === 'Enter' && e.ctrlKey) addNote();
    });

    document.getElementById('ht-change-name').addEventListener('click', function (e) {
      e.preventDefault();
      document.getElementById('ht-name-input').value = loadAuthor();
      $('#identityModal').modal('show');
    });

    window.addEventListener('popstate', function () {
      var h = getHashId();
      if (h) { renderDetail(h); } else { renderMain(); }
    });

    /* First visit check */
    if (!loadAuthor()) {
      $('#identityModal').modal('show');
    } else {
      boot();
    }
  });

}());
</script>
