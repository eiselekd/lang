<script >
  import Listelem from './Listelem.svelte';
  import {
      params
  } from "./store.js";
  import Cu from "./Cu.svelte";


  const examples = [
    { name: "param1", url: "/examples/param1.json" },
    { name: "param2", url: "/examples/param2.json" },
    { name: "param3", url: "/examples/param3.json" },
  ];

  
  async function loadExample(url) {
      if (!url) return;
      const response = await fetch(url);
      const example = await response.json();
      $params = example.params;
      console.log($params);
  }

</script>

<style>
</style>

<div class="examples">
  <label>Examples</label>
  <select on:change={(e) => loadExample(e.target.value)}>
    <option value="" />
    {#each examples as example}
      <option value={example.url}>{example.name}</option>
    {/each}
  </select>
</div>

<div> 
  <Listelem data={$params}  />
</div>


<h1>Hello2 !</h1>
